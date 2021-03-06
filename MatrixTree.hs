module MatrixTree
( addSubtreeNorms
, ifZeroReplace
, isZero
, matrixListToTree
, MatrixTree
, mmReadTree
, mmWriteTree
, MTree(..)
, nextPowOf2
, norm
, Norm
, setNorm
, size
, Size
, treeToMatrixList
, Value
, valueNorm
) where

-- a recursive matrix data type that efficiently encodes sparsity

import MatrixList (MatrixList, mEntryVal)
import MatrixMarket (mmReadFile, mmWriteFile)

type Size = Int ; type Value = Double ; type Norm = Double

data MTree = Zero Size |
             Leaf Norm Value |
             Square Size Norm MTree MTree MTree MTree
             deriving (Eq, Show)

type MatrixTree = (Int, Int, MTree)

size :: MTree -> Size
size (Zero s)             = s
size (Leaf _ _)           = 1
size (Square s _ _ _ _ _) = s

norm :: MTree -> Norm
norm (Zero _)             = 0
norm (Leaf n _)           = n
norm (Square _ n _ _ _ _) = n

subTrees :: MTree -> [MTree]
subTrees (Square _ _ tl tr bl br) = [tl, tr, bl, br]
subTrees _ = []

-- setting norms

setNorm :: MTree -> MTree
setNorm tree@(Zero _)             = tree
setNorm tree@(Leaf _ x)           = Leaf (valueNorm x) x
setNorm tree@(Square s _ _ _ _ _) = Square s n ntl ntr nbl nbr
        where [ntl, ntr, nbl, nbr] = fmap setNorm $ subTrees tree
              n = addSubtreeNorms [ntl, ntr, nbl, nbr]

valueNorm :: Value -> Norm
valueNorm = abs

addSubtreeNorms :: [MTree] -> Norm
addSubtreeNorms = sqrt . sum . fmap ((^2) . norm)

-- reading from/writing to MatrixMarket files

mmReadTree :: FilePath -> IO MatrixTree
mmReadTree filePath = mmReadFile filePath >>= (return . matrixListToTree)

mmWriteTree :: MatrixTree -> String -> FilePath -> IO ()
mmWriteTree tree format filePath =
            mmWriteFile (treeToMatrixList tree) format filePath

matrixListToTree :: MatrixList Double -> MatrixTree
matrixListToTree (h, w, ijxs) = (h, w, setNorm $ foldr addVal (Zero p) entries)
                                where p = nextPowOf2 $ max h w
                                      entries = filter ((/= 0) . mEntryVal) ijxs

addVal :: (Int, Int, Value) -> MTree -> MTree
addVal (_, _, x) (Leaf _ _) = Leaf 0 x
addVal (i, j, x) (Zero s)
 | s == 1         = Leaf 0 x
 | within [i,j]   = Square s 0 (addVal (i,  j,  x) zro) zro zro zro
 | within [i,jr]  = Square s 0 zro (addVal (i,  jr, x) zro) zro zro
 | within [ib,j]  = Square s 0 zro zro (addVal (ib,  j, x) zro) zro
 | within [ib,jr] = Square s 0 zro zro zro (addVal (ib, jr, x) zro)
 where halfs = s `div` 2
       within = all (<= halfs)
       ib = i - halfs ; jr = j - halfs
       zro = Zero halfs
addVal (i, j, x) (Square s _ tl tr bl br) = Square s 0 newtl newtr newbl newbr
       where [newtl, newtr, newbl, newbr]
                 | within [i,j]   = [addVal (i,  j,  x) tl, tr, bl, br]
                 | within [i,jr]  = [tl, addVal (i,  jr, x) tr, bl, br]
                 | within [ib,j]  = [tl, tr, addVal (ib,  j, x) bl, br]
                 | within [ib,jr] = [tl, tr, bl, addVal (ib, jr, x) br]
             within = all (<= halfs)
             ib = i - halfs ; jr = j - halfs
             halfs = s `div` 2

treeToMatrixList :: MatrixTree -> MatrixList Double
treeToMatrixList (h, w, mTree) = (h, w, mTreeToList mTree)

mTreeToList :: MTree -> [(Int, Int, Value)]
mTreeToList (Zero _)                  = []
mTreeToList (Leaf _ x)                = [(1, 1, x)]
mTreeToList tree@(Square s _ _ _ _ _) = concat [tlijxs, fmap wshift trijxs,
                                                fmap hshift blijxs,
                                                fmap (hshift . wshift) brijxs]
      where [tlijxs, trijxs, blijxs, brijxs] = fmap mTreeToList $ subTrees tree
            hshift (i, j, x) = (i + halfs, j, x)
            wshift (i, j, x) = (i, j + halfs, x)
            halfs = s `div` 2

-- utility functions

isZero :: MTree -> Bool
isZero (Zero _) = True
isZero _        = False

ifZeroReplace :: MTree -> MTree
ifZeroReplace tree@(Zero _)   = tree
ifZeroReplace tree@(Leaf _ x) = if x == 0 then Zero 1 else tree
ifZeroReplace tree@(Square s _ _ _ _ _)
                              = if all isZero (subTrees tree)
                                then Zero s else tree

nextPowOf2 :: Integral a => a -> a
nextPowOf2 n = head . dropWhile (< n) $ map (2^) [0..]