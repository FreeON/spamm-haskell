{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module MatrixMarket
( MMFormat
, MMList
, mmPack
, mmReadFile
, MMSym
, mmWriteMatrixFile
, mmWriteVectorFile
) where

-- reads/writes MatrixMarket files to generic matrix and vector data types

import Data.Char (toLower, toUpper)
import Data.Complex
import Data.List (intersperse, nub)
import qualified Data.Map as Map (fromList, lookup, Map)
import Data.Maybe (fromJust, isNothing)
--import Data.Typeable
import MatrixList
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import VectorList

-- mmReadFile takes a filepath and returns an MMList and the comments
-- as a single string; the user needs to unpack the MMList to
-- recover the MatrixList or VectorList containing the data

mmReadFile :: FilePath -> IO (MMList, String)
mmReadFile filePath = readFile filePath >>=
           (return . mmReadStr . filter (not . null . words) . lines)

-- mmWriteMatrixFile takes an MMList holding a MatrixList, a format,
-- a symmetry type, comments as a single string, and a filepath, and
-- writes everything to that file; the user needs to pack the MatrixList
-- using mmPack before calling this function

mmWriteMatrixFile :: MMList -> String -> MMFormat -> MMSym -> FilePath -> IO ()
mmWriteMatrixFile mmList comments format sym filePath
       | obj /= Matrix
         = error "WriteMatrixFile only writes matrices"
       | format == Array && field == Pattern
         = error "Array format can't store a Pattern"
       | sym == Hermitian && field /= Complex
         = error "Only complex matrices can be Hermitian"
       | otherwise
         = mmMatWrite mmList comments format sym filePath
       where (obj, field) = dataFromMMList mmList

-- mmWriteVectorFile takes an MMList holding a VectorList, a format,
-- comments as a single string, and a filepath, and writes everything
-- to that file; the user needs to pack the VectorList using mmPack
-- before calling this function

mmWriteVectorFile :: MMList -> String -> MMFormat -> FilePath -> IO ()
mmWriteVectorFile mmList comments format filePath
       | obj /= Vector
         = error "WriteVectorFile only writes vectors"
       | format == Array && field == Pattern
         = error "Array format can't store a Pattern"
       | otherwise
         = mmVecWrite mmList comments format filePath
       where (obj, field) = dataFromMMList mmList

-- exported data types and helper function

data MMFormat = Array | Coordinate deriving (Eq, Read, Show)

data MMSym = General | Symmetric | Hermitian | Skew deriving (Eq, Read, Show)

data MMList = MMMatInt (MatrixList Int) |
              MMMatReal (MatrixList Double) |
              MMMatComplex (MatrixList (Complex Double)) |
              MMMatPattern (MatrixList ()) |
              MMVecInt (VectorList Int) |
              MMVecReal (VectorList Double) |
              MMVecComplex (VectorList (Complex Double)) |
              MMVecPattern (VectorList ())

class MMPack a where
      mmPack :: a -> MMList

instance MMPack (MatrixList Int) where
         mmPack = MMMatInt

instance MMPack (MatrixList Double) where
         mmPack = MMMatReal

instance MMPack (MatrixList (Complex Double)) where
         mmPack = MMMatComplex

instance MMPack (MatrixList ()) where
         mmPack = MMMatPattern

instance MMPack (VectorList Int) where
         mmPack = MMVecInt

instance MMPack (VectorList Double) where
         mmPack = MMVecReal

instance MMPack (VectorList (Complex Double)) where
         mmPack = MMVecComplex

instance MMPack (VectorList ()) where
         mmPack = MMVecPattern

-- internal data types

data MMField  = Integer | Real | Complex | Pattern  deriving (Eq, Read, Show)
data MMObject = Matrix | Vector deriving (Eq, Read, Show)

-- read type, type class, and functions

class (Read a) => MMRead a where
      mmReads :: String -> [(a, String)]
      mmReads = reads . normalize

      mmRead :: String -> a
      mmRead = fst . head . mmReads

instance MMRead MMField where

instance MMRead MMFormat where

instance MMRead MMObject where

instance MMRead MMSym where
         mmReads sym = if fmap toLower sym == "skew-symmetric"
                       then [(Skew, "")] else reads $ normalize sym

mmReadStr :: [String] -> (MMList, String)
mmReadStr contents
          | null contents  = error "Empty file"
          | format == Array && field == Pattern
                           = error "Array format can't store a Pattern"
          | obj == Matrix  = mmReadMat format field headerRest rest
          | otherwise      = mmReadVec format field rest
          where first:rest = contents
                (obj, format, field, headerRest) = mmParseHeader first

mmParseHeader :: String -> (MMObject, MMFormat, MMField, [String])
mmParseHeader header
              | length hWords < 4 = error "Invalid header"
              | fmap toLower $ head hWords /= "%%matrixmarket"
                                  = error "Invalid header"
              | null objRead      = error "Invalid object type"
              | null formatRead   = error "Invalid format"
              | null fieldRead    = error "Invalid field"
              | otherwise         = (obj, format, field, rest)
              where hWords = words header
                    objRead = mmReads (hWords !! 1) :: [(MMObject, String)]
                    formatRead = mmReads (hWords !! 2) :: [(MMFormat, String)]
                    fieldRead = mmReads (hWords !! 3) :: [(MMField, String)]
                    obj = fst . head $ objRead
                    format = fst . head $ formatRead
                    field = fst . head $ fieldRead
                    rest = drop 4 hWords

mmReadMat :: MMFormat -> MMField -> [String] -> [String] -> (MMList, String)
mmReadMat format field headerRest rest = (mmList, comments)
          where sym = mmParseSym headerRest field
                (comments, contents) = mmReadComments (rest, [])
                mmList = case format of
                              Array      -> mmReadMatArr  sym field contents
                              Coordinate -> mmReadMatCoor sym field contents

mmReadVec :: MMFormat -> MMField -> [String] -> (MMList, String)
mmReadVec format field rest = (mmList, comments)
          where (comments, contents) = mmReadComments (rest, [])
                mmList = case format of
                              Array      -> mmReadVecArr  field contents
                              Coordinate -> mmReadVecCoor field contents

mmParseSym :: [String] -> MMField -> MMSym
mmParseSym rest field
           | null rest       = error "Invalid symmetry type"
           | null symRead    = error "Invalid symmetry type"
           | field /= Complex && sym == Hermitian
                             = error "Only complex matrices can be Hermitian"
           | otherwise       = sym
           where symRead = mmReads $ head rest :: [(MMSym, String)]
                 sym = fst . head $ symRead

mmReadComments :: ([String], [String]) -> Char -> ([String], String)
mmReadComments (text@(first:rest), comments) commentChar
      | head first == commentChar
                  = mmReadComments (rest, tail first:comments) commentChar
      | otherwise = (text, joinStr "\n" $ reverse comments)

mmReadMatArr :: MMSym -> MMField -> [String] -> MMList
mmReadMatArr sym field dimLine:valueLines
           | h /= w && sym /= General
                        = error "Matrix with this symmetry must be square"
           | otherwise  = mmListReadMatArr h w sym field valueLines
           where [h, w] = readInts 2 dimLine

mmReadMatCoor :: MMSym -> MMField -> [String] -> MMList
mmReadMatCoor sym field dimLine:valueLines
             | h /= w && sym /= General
                         = error "Matrix with this symmetry must be square"
             | nonzeros > maxNonzeros
                         = error "Too many nonzeros"
             | otherwise = mmListReadMatCoor h w nonzeros sym field valueLines
             where [h, w, nonzeros] = readInts 3 dimLine
                   maxNonzeros = case sym of General -> h * w
                                             Skew    -> h * (w - 1) `quot` 2
                                             _       -> h * (w + 1) `quot` 2

mmReadVecArr :: MMField -> [String] -> MMList
mmReadVecArr field dimLine:valueLines =
             mmListReadVecArr l field valueLines
             where l = head $ readInts 1 dimLine

mmReadVecCoor :: MMField -> [String] -> MMList
mmReadVecCoor field dimLine:valueLines
              | nonzeros > l = error "Too many nonzeros"
              | otherwise    = mmListReadVecCoor l nonzeros field valueLines
              where [l, nonzeros] = readInts 2 dimLine

readInts :: Int -> String -> [Int]
readInts n str
       | length intReads < n || any null intReads
                   = error "Invalid size line"
       | otherwise = fmap (fst . head) intReads
       where intReads = fmap reads . take n . words $ str :: [[(Int, String)]]


mmReadvalsfield size valueline:valueLines valuelist =
mmreadvalsfield (size - 1) valuelines valueentry:valuelist
where valueentry is parsed valueline
if valuelist is empty either finish or call error if size /= 1
if size == 0 either finish or call error if valuelist isn't empty
then there's the checking of the indices and the value according to symmetry

readsInt = reads :: String -> [(Int, String)]

readsReal = reads :: String -> [(Double, String)]

readsComplex = reads :: String -> [(Complex Double, String)]

readsPattern :: String -> [((), String)]
readsPattern _ = [(), ""]

{-
mmReadInt :: MMObj -> MMFormat -> String -> ListPair Int
                                         -> ListPair Int
mmReadInt obj format line (indices, ints)
  | intLen /= 1   = error "invalid entry"
  | null intReads = error "invalid entry"
  | otherwise     = (indices, val)
  where (indices, valLine) = mmReadIndices obj format line
        intLen = length (words valLine)
        intReads = reads valLine :: [(Int, String)]
        val = fst . head $ intReads

mmReadReal :: MMObj -> MMFormat -> String -> ListPair Double
                                          -> ListPair Double
mmReadReal obj format line (indices, reals)
  | realLen /= 1   = error "invalid entry"
  | null realReads = error "invalid entry"
  | otherwise      = (indices, val)
  where (indices, valLine) = mmReadIndices obj format line
        realLen = length (words valLine)
        realReads = reads valLine :: [(Double, String)]
        val = fst . head $ realReads

mmReadComplex :: MMObj -> MMFormat -> String -> ListPair (Complex Double)
                                             -> ListPair (Complex Double)
mmReadComplex obj format line (indices, cplxs)
  | valLen /= 2       = error "invalid entry"
  | null complexReads = error "invalid entry"
  | otherwise         = (indices, val)
  where (indices, valLine) = mmReadIndices obj format line
        valList = words valLine
        valLen = length valList
        complexReads = reads (joinStr " :+ " valList)
                             :: [(Complex Double, String)]
        val = fst . head $ complexReads

mmReadPattern :: MMObj -> MMFormat -> String -> ListPair ()
                                             -> ListPair ()
mmReadPattern obj format line (indices, patterns)
  | not (null valLine) = error "invalid entry"
  | otherwise          = (indices, ())
  where (indices, valLine) = mmReadIndices obj format line

mmReadIndices :: MMObj -> MMFormat -> String -> ([Int], String)
mmReadIndices obj format line = case format of
                                     Array      -> ([], line)
                                     Coordinate -> mmReadCoor obj line

mmReadCoor :: MMObj -> String -> ([Int], String)
mmReadCoor obj line
           | length lineWords < n    = error "invalid entry"
           | any . null $ indexReads = error "invalid entry"
           | otherwise               = (indices, rest)
           where lineWords = words line
                 n = if obj == Vector then 1 else 2
                 (first, rest) = (splitAt n) lineWords
                 indexReads = fmap reads first :: [[(Int, String)]]
                 indices = fmap (fst . head) indexReads

matArrIndexList :: Int -> Int -> MMSym -> [[Int]]
matArrIndexList h w sym =
                case sym of General -> ijs
                            Skew    -> filter (\[i, j] -> i > j) ijs
                            _       -> filter (\[i, j] -> i >= j) ijs
                where ijs = [[i, j] | j <- [1..w], i <- [1..h]]

vecArrIndexList :: Int -> [[Int]]
vecArrIndexList length = [[i] | i <- [1..length]]




valueLineLen :: MMObj -> MMFormat -> Int
valueLineLen _      Array      = 1
valueLineLen Vector Coordinate = 2
valueLineLen Matrix Coordinate = 3

readFieldCmds :: Map.Map MMField ((MMSym -> [[String]] -> IndexRead) ->
                                   MMSym -> [[String]] -> MMList)
readFieldCmds = Map.fromList [
                (Integer, mmReadInt)
              , (Real, mmReadReal)
              , (Complex, mmReadComplex)
              ]

readFormatCmds :: Map.Map MMFormat (MMSym -> [[String]] -> IndexRead)
readFormatCmds = Map.fromList [
                 (Array, mmReadArr)
               , (Coordinate, mmReadCoor)
               ]

              fst_ijxs = fmap (\((i, j), x) -> (i, j, x)) .
                         filter ((/= 0) . snd) $ zip ijs values
              ijxs = completeNumList sym fst_ijxs



           allijs = [(i, j) | j <- [1..w], i <- [1..h]]
           ijs = case sym of General -> allijs
                             Skew    -> filter (\(i, j) -> i > j) allijs
                             _       -> filter (\(i, j) -> i >= j) allijs

     | nonzeros == 0                    = (h, w, [], [])
     | any (any null) ijReads           = error "list has invalid indices"
     | ijs /= nub ijs                   = error "list has duplicate indices"
     | not $ ijsValid sym               = error "invalid indices for matrix type"
     | maxRow > h || maxCol > w = error "list has indices outside range"
     | otherwise                        = (h, w, ijs, valueList)
     where firstLine:entryLines = lineList
           firstLineNums = fmap reads firstLine :: [[(Int, String)]]
           [h, w, nonzeros] = fmap (fst . head) firstLineNums
           splitEntries = fmap (splitAt 2) entryLines
           ijReads = fmap (fmap reads . fst) splitEntries :: [[[(Int, String)]]]
           ijs = fmap ((\[i, j] -> (i, j)) . fmap (fst . head)) ijReads
           ijsValid General = True
           ijsValid Skew    = all (\(i, j) -> i > j) ijs
           ijsValid _       = all (\(i, j) -> i >= j) ijs
           [maxRow, maxCol] = fmap (maximum . ($ unzip ijs)) [fst, snd]
           valueList = concat . fmap snd $ splitEntries

validNumList :: (Num a) => MMSym -> [(Int, Int, a)] -> Bool
validNumList = undefined

validCompList :: (Num a) => MMSym -> [(Int, Int, Complex a)] -> Bool
validCompList = undefined

completeNumList :: (Num a) => MMSym -> [(Int, Int, a)] -> [(Int, Int, a)]
completeNumList = undefined

completeCompList :: (Num a) => MMSym -> [(Int, Int, Complex a)] -> [(Int, Int, Complex a)]
completeCompList = undefined

     | sym == Hermitian && not .
       all ((== 0) . imagPart) diag     = error "diagonal entries aren't real"
           diag = fmap (\(_, _, MMMatComplex x) -> x) .
                  filter (\(i, j, _) -> i == j) $ initijxs


completeList :: MMSym -> [MListEntry MMValue] -> [MListEntry MMValue]
completeList sym ijxs = ijxs ++ moreijxs sym
             where moreijxs General   = []
                   moreijxs Skew      = fmap (\(i, j, x) -> (j, i, neg x)) ijxs
                   moreijxs Symmetric = fmap (\(i, j, x) -> (j, i, x)) .
                                        filter (\(i, j, x) -> i /= j) $ ijxs
                   moreijxs Hermitian = fmap (\(i, j, x) -> (j, i, conj x)) .
                                        filter (\(i, j, x) -> i /= j) $ ijxs
                   neg (MMMatInt n)     = MMMatInt (-n)
                   neg (MMMatReal x)    = MMMatReal (-x)
                   neg (MMMatComplex z) = MMMatComplex (-z)
                   conj (MMMatComplex z) = MMMatComplex (conjugate z)
-}
-- write type class and functions

class (Show a) => MMShow a where
      mmShow :: a -> String
      mmShow = fmap toLower . show

instance MMShow MMField where

instance MMShow MMFormat where

instance MMShow MMSym where
         mmShow sym = if sym == Skew then "skew-symmetric"
                      else fmap toLower $ show sym

instance MMShow Int where

instance MMShow Double where

instance (MMShow a, RealFloat a) => MMShow (Complex a) where
         mmShow z = joinStr " " . fmap mmShow $ [realPart z, imagPart z]

instance MMShow () where
         mmShow _ = ""

dataFromMMList :: MMList -> (MMObject, MMField)
dataFromMMList (MMMatInt _)     = (Matrix, Integer)
dataFromMMList (MMMatReal _)    = (Matrix, Real)
dataFromMMList (MMMatComplex _) = (Matrix, Complex)
dataFromMMList (MMMatPattern _) = (Matrix, Pattern)
dataFromMMList (MMVecInt _)     = (Vector, Integer)
dataFromMMList (MMVecReal _)    = (Vector, Real)
dataFromMMList (MMVecComplex _) = (Vector, Complex)
dataFromMMList (MMVecPattern _) = (Vector, Pattern)

mmMatWrite :: MMList -> String -> MMFormat -> MMSym -> FilePath -> IO ()
mmMatWrite (MMMatInt mList) comments format sym filePath =
           if hasSym sym mList then
           mmMatWriteField (symFilter sym $ mRmZeros mList) comments
                           "0" format Integer sym filePath
           else error "Matrix doesn't have right symmetry type"
mmMatWrite (MMMatReal mList) comments format sym filePath =
           if hasSym sym mList then
           mmMatWriteField (symFilter sym $ mRmZeros mList) comments
                           "0.0" format Real sym filePath
           else error "Matrix doesn't have right symmetry type"
mmMatWrite (MMMatComplex mList) comments format sym filePath =
           if hasSymComplex sym mList then
           mmMatWriteField (symFilter sym $ mRmZeros mList) comments
                           "0.0 0.0" format Complex sym filePath
           else error "Matrix doesn't have right symmetry type"
mmMatWrite (MMMatPattern mList) comments format sym filePath =
           mmMatWriteField (symFilter sym mList) comments
                           "" format Pattern sym filePath

hasSym :: (Eq a, Num a) => MMSym -> MatrixList a -> Bool
hasSym General   _     = True
hasSym Symmetric mList = isSym mList
hasSym Skew      mList = isSkew mList
hasSym _         _     = False

hasSymComplex :: (Eq a, RealFloat a) => MMSym -> MatrixList (Complex a) -> Bool
hasSymComplex General   _     = True
hasSymComplex Symmetric mList = isSym mList
hasSymComplex Skew      mList = isSkew mList
hasSymComplex Hermitian mList = isHerm mList

symFilter :: MMSym -> MatrixList a -> MatrixList a
symFilter General mList        = mList
symFilter Skew    (h, w, ijxs) = (h, w, filter (\(i, j, _) -> i > j)  ijxs)
symFilter _       (h, w, ijxs) = (h, w, filter (\(i, j, _) -> i >= j) ijxs)

mmMatWriteField :: MMShow a => MatrixList a -> String -> String ->
                               MMFormat -> MMField -> MMSym ->
                               FilePath -> IO ()
mmMatWriteField mList comments zroStr format field sym filePath =
     if not (validMat mList) then
     error "Matrix has invalid indices"
     else
     do handle <- openFile filePath WriteMode
        let hWrite = hPutStrLn handle
        hWrite $ mmMatHeader format field sym
        mapM_ hWrite $ mmComments comments
        let (h, w, ijxs) = mList
        hWrite $ joinStr " " . fmap show $ [h, w] ++
                 case format of Array      -> []
                                Coordinate -> [length ijxs]
        mapM_ hWrite $ mValList mList zroStr format sym
        hClose handle

validMat :: MatrixList a -> Bool
validMat (h, w, ijxs) = h >= (maxVal mEntryRow) ijxs &&
                        w >= (maxVal mEntryCol) ijxs &&
                        ijs == nub ijs
                        where maxVal f = safeMax . fmap f
                              ijs = fmap (\(i, j, _) -> (i, j)) ijxs

mmMatHeader :: MMFormat -> MMField -> MMSym -> String
mmMatHeader format field sym =
            joinStr " " ["%%MatrixMarket matrix", mmShow format, mmShow field,
                         mmShow sym]

mValList :: MMShow a => MatrixList a -> String -> MMFormat -> MMSym -> [String]
mValList (h, w, ijxs) zroStr format sym =
         case format of Array      -> fmap findVal ijs
                        Coordinate -> fmap showEntry ijxs
         where ijs = ijList h w sym
               hashTable = Map.fromList $ fmap (\(i, j, x) -> ((i,j), x)) ijxs
               findVal pair | isNothing check = zroStr
                            | otherwise       = mmShow $ fromJust check
                            where check = Map.lookup pair hashTable
               showEntry (i, j, x) = joinStr " " [show i, show j, mmShow x]

ijList :: Int -> Int -> MMSym -> [(Int, Int)]
ijList h w sym = case sym of General -> [(i, j) | j <- [1..w], i <- [1..h]  ]
                             Skew    -> [(i, j) | j <- [1..w], i <- [j+1..h]]
                             _       -> [(i, j) | j <- [1..w], i <- [j..h]  ]

mmVecWrite :: MMList -> String -> MMFormat -> FilePath -> IO ()
mmVecWrite (MMVecInt vList) comments format filePath =
  mmVecWriteField (vRmZeros vList) comments "0" format Integer filePath
mmVecWrite (MMVecReal vList) comments format filePath =
  mmVecWriteField (vRmZeros vList) comments "0.0" format Real filePath
mmVecWrite (MMVecComplex vList) comments format filePath =
  mmVecWriteField (vRmZeros vList) comments "0.0 0.0" format Complex filePath
mmVecWrite (MMVecPattern vList) comments format filePath =
  mmVecWriteField vList comments "" format Pattern filePath

mmVecWriteField :: MMShow a => VectorList a -> String -> String ->
                               MMFormat -> MMField -> FilePath -> IO ()
mmVecWriteField vList comments zroStr format field filePath =
     if not (validVec vList) then
     error "Vector has invalid indices"
     else
     do handle <- openFile filePath WriteMode
        let hWrite = hPutStrLn handle
        hWrite $ mmVecHeader format field
        mapM_ hWrite $ mmComments comments
        let (l, ixs) = vList
        hWrite $ joinStr " " . fmap show $ [l] ++
                 case format of Array      -> []
                                Coordinate -> [length ixs]
        mapM_ hWrite $ vValList vList zroStr format
        hClose handle

validVec :: VectorList a -> Bool
validVec (l, ixs) = l >= (maxVal vEntryIndex) ixs &&
                   is == nub is
                   where maxVal f = safeMax . fmap f
                         is = fmap (\(i,x) -> i) ixs

mmVecHeader :: MMFormat -> MMField -> String
mmVecHeader format field =
            joinStr " " ["%%MatrixMarket vector", mmShow format, mmShow field]

vValList :: MMShow a => VectorList a -> String -> MMFormat -> [String]
vValList (l, ixs) zroStr format =
         case format of Array      -> fmap findVal [1..l]
                        Coordinate -> fmap showEntry ixs
         where hashTable = Map.fromList ixs
               findVal i | isNothing check = zroStr
                         | otherwise       = mmShow $ fromJust check
                         where check = Map.lookup i hashTable
               showEntry (i, x) = joinStr " " [show i, mmShow x]

mmComments :: String -> [String]
mmComments = fmap (commentChar:) . lines

-- constants and utility functions

commentChar = '%' :: Char

normalize :: String -> String
normalize ""  = ""
normalize (x:xs) = (toUpper x):(fmap toLower xs)

joinStr :: String -> [String] -> String
joinStr s = concat . intersperse s

safeMax :: (Num a, Ord a) => [a] -> a
safeMax xs = if null xs then 0 else maximum xs
{-
typeEq :: (Typeable a, Typeable b) => a -> b -> Bool
typeEq x y = typeOf x == typeOf y
-}
