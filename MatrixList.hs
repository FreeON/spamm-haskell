module MatrixList
( isHerm
, isSkew
, isSym
, MatrixList
, mEntryCol
, mEntryLoc
, mEntryRow
, mEntryVal
, MListEntry
, mListEntries
, mListHeight
, mListWidth
, mRmZeros
) where

import Data.Complex
import qualified Data.Map as Map (fromList, lookup, Map)
import Data.Maybe (fromJust, isNothing)

-- matrix height, width, and list of entries (row, column, value) ;
-- including zero-value entries is optional

type MListEntry a = (Int, Int, a)
type MatrixList a = (Int, Int, [MListEntry a])

mListHeight :: MatrixList a -> Int
mListHeight (h, _, _) = h

mListWidth :: MatrixList a -> Int
mListWidth (_, w, _) = w

mListEntries :: MatrixList a -> [MListEntry a]
mListEntries (_, _, ijxs) = ijxs

mEntryLoc :: MListEntry a -> (Int, Int)
mEntryLoc (i, j, _) = (i, j)

mEntryRow :: MListEntry a -> Int
mEntryRow = fst . mEntryLoc

mEntryCol :: MListEntry a -> Int
mEntryCol = snd . mEntryLoc

mEntryVal :: MListEntry a -> a
mEntryVal (_, _, x) = x

mRmZeros :: (Eq a, Num a) => MatrixList a -> MatrixList a
mRmZeros (h, w, ijxs) = (h, w, filter ((/= 0) . mEntryVal) ijxs)

isSym :: Eq a => MatrixList a -> Bool
isSym (h, w, ijxs) = h == w && all sym ijs
      where ijs = filter (\(i, j) -> i > j) $ fmap mEntryLoc ijxs
            sym (i, j) = if isNothing mji then False else mij == mji
                where mij = mFindVal ijxs (i, j)
                      mji = mFindVal ijxs (j, i)

isSkew :: (Eq a, Num a) => MatrixList a -> Bool
isSkew (h, w, ijxs) = h == w && all skewSym ijs
       where ijs = filter (\(i, j) -> i >= j) $ fmap mEntryLoc ijxs
             skewSym (i, j) = if isNothing mji then False
                              else fromJust mij == -fromJust mji
                     where mij = mFindVal ijxs (i, j)
                           mji = mFindVal ijxs (j, i)

isHerm :: (Eq a, RealFloat a) => MatrixList (Complex a) -> Bool
isHerm (h, w, ijxs) = h == w && all herm ijs
       where ijs = filter (\(i, j) -> i >= j) $ fmap mEntryLoc ijxs
             herm (i, j) = if isNothing mji then False
                           else fromJust mij == conjugate (fromJust mji)
                  where mij = mFindVal ijxs (i, j)
                        mji = mFindVal ijxs (j, i)

mFindVal :: [MListEntry a] -> (Int, Int) -> Maybe a
mFindVal ijxs pair = Map.lookup pair $ Map.fromList $
                     fmap (\(i, j, x) -> ((i,j), x)) ijxs
