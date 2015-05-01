{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Data types used by `Filediff`
module Filediff.Types
( Filediff(..)
, Diff(..)
, FileChange(..)
, ListDiff(..)
, listDiff
, isDel
, isMod
, isAdd
, Line
, Error
) where

import GHC.Generics

import Data.Default
import Data.Function (on)

import qualified Data.Text as T

import Data.Maybe

import Zora.List (merge, merge_by)
import Data.List (find, intersect, intersectBy, sortBy, (\\))

import Data.Monoid
import Control.Applicative

import Data.MemoCombinators (Memo, wrap)
import Data.MemoCombinators.Class (MemoTable, table, memoize)

-- | Diff between two lists. `dels` represents the indices
--   at which to delete, and `adds` represents the indices and
--   contents to add.
data ListDiff a = ListDiff
    { dels :: [(Int, a)]
    , adds :: [(Int, a)] }
    deriving (Show, Eq, Generic)

instance Default (ListDiff a) where
    def :: ListDiff a
    def = ListDiff [] []

instance (Eq a, Ord a, MemoTable a) => Monoid (ListDiff a) where
    mempty :: ListDiff a
    mempty = ListDiff [] []

    -- may fail
    mappend :: ListDiff a -> ListDiff a -> ListDiff a
    mappend
        (ListDiff abDels abAdds)
        (ListDiff bcDels bcAdds)
        = ListDiff acDels acAdds
        where
            acDels :: [(Int, a)]
            acDels = merge abDels bDelsFromA

            -- indices (in `a`) of elements that survive (a -> b)
            -- , but not (b -> c)
            -- TODO: `intersectBy` almost certainly ain't linear.
            -- Should probably write it here since we know these
            -- are sorted.
            bDelsFromA :: [(Int, a)]
            bDelsFromA
                = catMaybes
                . map f
                $ bcDels
                where
                    f :: (Int, a) -> Maybe (Int, a)
                    f (bi, ch) =
                        case match of
                            Just (ai, _) -> Just (ai, ch)
                            Nothing -> Nothing
                        where
                            match :: Maybe (Int, Int)
                            match = find (\(sai, sbi) -> sbi == bi) survivingAIndicesInB


                    --(\(_, bi) (_, biDeleted) -> bi == biDeleted)
                    --(zip survivingAIndicesInB) --:: [(Int, Int!)]
                    --(bcDels) 

            -- indices (in b) of elements that survive (a -> b)
            -- (in format [(in a, in b)])
            survivingAIndicesInB :: [(Int, Int)]
            survivingAIndicesInB = map (\(b,a) -> (a,b)) $ indicesAfterAdds 0 survivingAIndices (map fst abAdds)

            -- will not be all if the last elem of `a` is
            -- not deleted, but doesn't make a difference
            survivingAIndices :: [Int]
            survivingAIndices = if null abDels
                then []
                else [0..(maximum abDels')] \\ abDels'
                where
                    abDels' :: [Int]
                    abDels' = map fst abDels

            -- TODO: WEIRD. not using `forall a.` but still needs to be `b`?
            -- Given elements and their indices as [(Int, b)] as the only
            -- elements to survive the transformation, and [Int] as the
            -- indices added in the transformation, calculate the eventual
            -- positions of the elements.
            indicesAfterAdds :: Int -> [b] -> [Int] -> [(Int, b)]
            indicesAfterAdds _ [] _ = []
            indicesAfterAdds i elems@(x:xs) [] = (:) (i, x) $ indicesAfterAdds (i + 1) xs []
            indicesAfterAdds i elems@(x:xs) adds@(a:as) =
                if i < a
                    then (:) (i, x) $ indicesAfterAdds (i + 1) xs (a:as)
                    else indicesAfterAdds (i + 1) (x:xs) as

            acAdds :: [(Int, a)]
            acAdds = merge_by (\(i,_) (j,_) -> i `compare` j) bcAdds cAddsFromA

            cAddsFromA :: [(Int, a)]
            cAddsFromA = indicesAfterAdds 0 (map snd survivingABAdds) (map fst bcAdds)

            -- adds in (a -> b) that survive (b -> c)
            survivingABAdds :: [(Int, a)]
            survivingABAdds = survivingABAdds' abAdds bcDels

            survivingABAdds' :: [(Int, a)] -> [(Int, a)] -> [(Int, a)]
            survivingABAdds' [] _ = []
            survivingABAdds' adds [] = adds
            survivingABAdds' (a:adds) (d:dels) =
                case (fst a) `compare` (fst d) of
                    LT -> (:) a $ survivingABAdds' adds (d:dels)
                    EQ -> survivingABAdds' adds dels
                    GT -> survivingABAdds' (a:adds) dels

-- | The basic data type for a difference between two files. The
--   "base" `FilePath` is the file chose state is being compared against,
--   and the "comp" `FilePath` is the file being compared (the "later"
--   of the two).
data Filediff = Filediff {
    base :: FilePath,
    comp :: FilePath,
    change :: FileChange
} deriving (Eq, Show, Generic)

-- | The types and sets of changes possible between two files.
data FileChange
    = Del (ListDiff Line)
    | Mod (ListDiff Line)
    | Add (ListDiff Line) deriving (Eq, Show, Generic)

-- | Gets the 'ListDiff' stored in a 'FileChange'.
listDiff :: FileChange -> ListDiff Line
listDiff (Del diff) = diff
listDiff (Mod diff) = diff
listDiff (Add diff) = diff

-- | Whether a 'FileChange' is a deletion or not.
isDel :: FileChange -> Bool
isDel (Del _) = True
isDel (Mod _) = False
isDel (Add _) = False
-- | Whether a 'FileChange' is a modification or not.
isMod :: FileChange -> Bool
isMod (Del _) = False
isMod (Mod _) = True
isMod (Add _) = False
-- | Whether a 'FileChange' is a addition or not.
isAdd :: FileChange -> Bool
isAdd (Del _) = False
isAdd (Mod _) = False
isAdd (Add _) = True

instance Monoid FileChange where
    mempty :: FileChange
    mempty = Mod mempty -- no changes (no add / del); identity diff

    mappend :: FileChange -> FileChange -> FileChange
    mappend (Del _    ) (Del _    ) = error "del ++ del"
    mappend (Del _    ) (Mod _    ) = error "del ++ mod"
    mappend (Del diff1) (Add diff2) = Mod $ diff1 `mappend` diff2
    mappend (Mod _    ) (Del diff2) = Del diff2
    mappend (Mod diff1) (Mod diff2) = Mod $ diff1 `mappend` diff2
    mappend (Mod _    ) (Add _    ) = error "mod ++ add"
    mappend (Add _    ) (Del _    ) = mempty -- will be filtered out during directory composition. Yes; this isn't ideal, but it's at least clean.
    mappend (Add diff1) (Mod diff2) = Add $ diff1 `mappend` diff2
    mappend (Add _    ) (Add _    ) = error "add ++ add"

-- TODO: is this mathematically correct?
instance Monoid Filediff where
    mempty :: Filediff
    mempty = Filediff "" "" (Mod mempty)

    mappend :: Filediff -> Filediff -> Filediff
    mappend fd1 fd2 =
        if comp fd1 /= base fd2
            then error $ "`comp` of filediff 1 is not `base` of filediff 2: " ++ (show fd1) ++ " vs. " ++ (show fd2) 
            else Filediff {
                base = base fd1,
                comp = comp fd2,
                change = change fd1 `mappend` change fd2 }

-- | A data type for differences between directories. `filediffs`
--   stores 'Filediffs` whose filepaths are relative to directories being
--   diffed.
data Diff = Diff {
    filediffs :: [Filediff]
} deriving (Show, Generic)

instance Eq Diff where
    (==) :: Diff -> Diff -> Bool
    (==) a b
        = sortBy cmp (filediffs a) == sortBy cmp (filediffs b)
        where
            cmp :: Filediff -> Filediff -> Ordering
            cmp a b = if base a /= base b
                then base a `compare` base b
                else comp a `compare` comp b

instance Default Diff where
    def :: Diff
    def = Diff []

instance MemoTable T.Text where
    table :: Memo T.Text
    table = wrap T.pack T.unpack table

instance Monoid Diff where
    mempty :: Diff
    mempty = Diff []

    mappend :: Diff -> Diff -> Diff
    mappend (Diff aFilediffs) (Diff bFilediffs)
        = Diff $ filediffs
        where
            filediffs :: [Filediff]
            filediffs
                = filter ((/=) mempty . change)
                $ exclusion ++ (map (uncurry mappend) intersection)

            exclusion :: [Filediff]
            exclusion
                =  (excludeBy dirsEqual aFilediffs bFilediffs)
                ++ (excludeBy dirsEqual bFilediffs aFilediffs)

            intersection :: [(Filediff, Filediff)]
            intersection = intersectBy dirsEqual aFilediffs bFilediffs

            dirsEqual :: Filediff -> Filediff -> Bool
            dirsEqual
                (Filediff aBase aComp _)
                (Filediff bBase bComp _)
                = (aBase == bBase) && (aComp == bComp)

            excludeBy  :: (a -> a -> Bool) -> [a] -> [a] -> [a]
            excludeBy _ [] _ = []
            excludeBy f (x:xs) ys =
                if any (f x) ys
                    then excludeBy f xs ys
                    else x : excludeBy f xs ys

            intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [(a, a)]
            intersectBy f a b
                = filter (uncurry f)
                $ (\x y -> (x,y)) <$> a <*> b

-- | Data type for a line.
type Line = T.Text

-- | Basic error type.
type Error = String
