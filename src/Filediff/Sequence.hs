{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Diffing algorithms (all exposed functions are pure)
module Filediff.Sequence
( -- * data types
  SeqDiff(..)

  -- * list operations
, diffSequences
, applySequenceDiff
) where

import Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo

import Data.List ((\\), sort, intersectBy)

import Zora.List (merge, merge_by)

import Data.Monoid

-- * data types

-- | Diff between two sequences. `fst` represents the indices
-- | at which to delete, and `snd` represents the indices and
-- | contents to add.
data SeqDiff a = SeqDiff {
      dels :: [Int]
    , adds :: [(Int, a)] }
    deriving (Show, Eq)

instance (Eq a, MemoTable a) => Monoid (SeqDiff a) where
    mempty :: SeqDiff a
    mempty = SeqDiff [] []

    -- may fail
    mappend :: SeqDiff a -> SeqDiff a -> SeqDiff a
    mappend
        (SeqDiff abDels abAdds)
        (SeqDiff bcDels bcAdds)
        = SeqDiff acDels acAdds
        where
            acDels :: [Int]
            acDels = merge abDels bDelsFromA

            -- indices of elements that survive (a -> b)
            -- , but not (b -> c)
            -- TODO: `intersectBy` almost certainly ain't linear.
            -- Should probably write it here.
            bDelsFromA :: [Int]
            bDelsFromA
                = map fst $ intersectBy
                    (\(ai, bi) (_, biDeleted) -> bi == biDeleted)
                    aIndicesInB 
                    $ zip (repeat 0) bcDels -- fst doesn't matter

            -- indices (in b) of elements that survive (a -> b)
            -- (in format [(in a, in b)])
            aIndicesInB :: [(Int, Int)]
            aIndicesInB = map (\(b,a) -> (a,b)) $ indicesAfterAdds 0 survivingAIndices (map fst abAdds)

            -- will not be all if the last elem of `a` is
            -- not deleted, but doesn't make a difference
            survivingAIndices :: [Int]
            survivingAIndices = [0..(maximum abDels)] \\ abDels

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

            survivingABAdds' :: [(Int, a)] -> [Int] -> [(Int, a)]
            survivingABAdds' [] _ = []
            survivingABAdds' (a:adds) (d:dels) =
                case (fst a) `compare` d of
                    LT -> (:) a $ survivingABAdds' adds (d:dels)
                    EQ -> survivingABAdds' adds dels
                    GT -> survivingABAdds' (a:adds) dels

-- * list operations

-- | returns (to delete, to add)
-- |
-- |     > diffSequences "abcdefg" "wabxyze"
-- |     ([2,3,5,6],[(0,'w'),(3,'x'),(4,'y'),(5,'z')])
diffSequences :: forall a. (Eq a, MemoTable a) => [a] -> [a] -> SeqDiff a
diffSequences a b = SeqDiff
    (nonSubsequenceIndices common a)
    (getProgressiveIndicesToAdd common b)
    where
        common :: [a]
        common = longestCommonSubsequence a b

        -- | λ add
        -- | [(0,"w"),(3,"x"),(4,"y")]
        -- | λ common
        -- | ["a","b","e"]
        getProgressiveIndicesToAdd :: (Eq a) => [a] -> [a] -> [(Int, a)]
        getProgressiveIndicesToAdd sub super =
            map (\i -> (i, super !! i)) $ nonSubsequenceIndices sub super

applySequenceDiff :: forall a. (Eq a) => SeqDiff a -> [a] -> [a]
applySequenceDiff (SeqDiff dels adds)
    = insertAtProgressiveIndices adds . removeAtIndices dels
    where
        -- | Best explained by example:
        -- |
        -- |     > insertAtProgressiveIndices [(1,'a'),(3,'b')] "def"
        -- |     "daebf"
        insertAtProgressiveIndices :: [(Int, a)] -> [a] -> [a]
        insertAtProgressiveIndices = insertAtProgressiveIndices' 0

        insertAtProgressiveIndices' :: Int -> [(Int, a)] -> [a] -> [a]
        insertAtProgressiveIndices' _ [] dest = dest
        insertAtProgressiveIndices' curr src@((i,s):src') dest@(d:dest') =
            if i == curr
                then s : insertAtProgressiveIndices' (succ curr) src' dest
                else d : insertAtProgressiveIndices' (succ curr) src dest'

-- all functions below are not exposed

-- optimization: hash lines
-- | Compute the longest common (potentially noncontiguous) subsequence
-- | between two sequences. Element type is fixed because memoization
-- | requires a static type.
longestCommonSubsequence :: forall a. (MemoTable a, Eq a) =>
                                [a] -> [a] -> [a]
longestCommonSubsequence
    = Memo.memo2
        (Memo.list table)
        (Memo.list table)
        longestCommonSubsequence'
    where
        longestCommonSubsequence' :: [a] -> [a] -> [a]
        longestCommonSubsequence' [] _ = []
        longestCommonSubsequence' _ [] = []
        longestCommonSubsequence' (x:xs) (y:ys) =
            if x == y
                then x : (longestCommonSubsequence xs ys) -- WLOG
                else if (length caseX) > (length caseY)
                    then caseX
                    else caseY
            where
                caseX :: [a]
                caseX = longestCommonSubsequence xs (y:ys)

                caseY :: [a]
                caseY = longestCommonSubsequence (x:xs) ys

-- | When `sub` is a (not necessarily contiguous) subsequence of `super`,
-- | get the index at which each element of `sub` appears. E.g.
-- |
-- |     > subsequenceIndices "abe" "abcdefg"
-- |     [0,1,4]
subsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
subsequenceIndices [] _ = []
subsequenceIndices _ [] = error "`sub` was not a subsequence of `super`"
subsequenceIndices sub@(a:sub') super@(b:super') =
    if a == b
        then 0 : map succ (subsequenceIndices sub' super')
        else     map succ (subsequenceIndices sub super')

-- | When `sub` is a (not necessarily contiguous) subsequence of `super`,
-- | get the indices at which elements of `sub` do *not* appear. E.g.
-- |
-- |     > nonSubsequenceIndices "abe" "abcdefg"
-- |     [2,3,5,6]
nonSubsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
nonSubsequenceIndices sub super =
    [0..(length super - 1)] \\ (subsequenceIndices sub super)

-- | /O(n)/. `indices` parameter *must* be sorted in increasing order,
-- | and indices must all exist
removeAtIndices :: forall a. [Int] -> [a] -> [a]
removeAtIndices = removeAtIndices' 0
    where
        removeAtIndices' :: Int -> [Int] -> [a] -> [a]
        removeAtIndices' _ [] xs = xs
        removeAtIndices' curr (i:is) (x:xs) =
            if curr == i
                then     removeAtIndices' (succ curr) is xs
                else x : removeAtIndices' (succ curr) (i:is) xs
