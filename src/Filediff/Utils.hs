module Filediff.Utils
( -- * list operations
  longestCommonSubsequence
, subsequenceIndices
, nonSubsequenceIndices
) where

import Data.List ((\\))

import qualified Data.MemoCombinators as Memo

import Filediff.Types

-- optimization: hash lines
longestCommonSubsequence :: [Line] -> [Line] -> [Line]
longestCommonSubsequence
    = Memo.memo2
        (Memo.list $ Memo.list Memo.char)
        (Memo.list $ Memo.list Memo.char)
        longestCommonSubsequence'
    where
        longestCommonSubsequence' :: [Line] -> [Line] -> [Line]
        longestCommonSubsequence' [] _ = []
        longestCommonSubsequence' _ [] = []
        longestCommonSubsequence' (x:xs) (y:ys) =
            if x == y
                then x : (longestCommonSubsequence xs ys) -- WLOG
                else if (length caseX) > (length caseY)
                    then caseX
                    else caseY
            where
                caseX :: [Line]
                caseX = longestCommonSubsequence xs (y:ys)

                caseY :: [Line]
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
