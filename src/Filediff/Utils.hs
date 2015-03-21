{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions not to be exposed by `Filediff`
module Filediff.Utils
( -- * list operations
  removeAtIndices
, longestCommonSubsequence
, subsequenceIndices
, nonSubsequenceIndices
) where

import Data.List ((\\))

import Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo

import Filediff.Types

-- optimization: hash lines
-- | Compute the longest common (potentially noncontiguous) subsequence
-- | between two sequences. Element type is fixed because memoization
-- | requires a static type.
longestCommonSubsequence :: forall a. (MemoTable a, Eq a) => [a] -> [a] -> [a]
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
removeAtIndices :: [Int] -> [a] -> [a]
removeAtIndices = removeAtIndices' 0
    where
        removeAtIndices' :: Int -> [Int] -> [a] -> [a]
        removeAtIndices' _ [] xs = xs
        removeAtIndices' curr (i:is) (x:xs) =
            if curr == i
                then     removeAtIndices' (succ curr) is xs
                else x : removeAtIndices' (succ curr) (i:is) xs
