module VectorList
( VectorList
, vEntryIndex
, vEntryVal
, VListEntry
, vListEntries
, vListLength
, vRmZeros
) where

-- vector length and list of entries (index, value) ;
-- including zero-value entries is optional

type VListEntry a = (Int, a)
type VectorList a = (Int, [VListEntry a])

vListLength :: VectorList a -> Int
vListLength (l, _) = l

vListEntries :: VectorList a -> [VListEntry a]
vListEntries (_, ixs) = ixs

vEntryIndex :: VListEntry a -> Int
vEntryIndex (i, _) = i

vEntryVal :: VListEntry a -> a
vEntryVal (_, x) = x

vRmZeros :: (Eq a, Num a) => VectorList a -> VectorList a
vRmZeros (l, ixs) = (l, filter ((/= 0) . vEntryVal) ixs)
