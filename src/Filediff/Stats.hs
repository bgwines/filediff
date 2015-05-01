{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | Statistics on diffs
module Filediff.Stats
( -- * basic stats
  numFilesAffected
, numAddedLines
, numDeletedLines
) where

import Filediff.Types

-- | Number of files added, modified, or deleted in a diff.
numFilesAffected :: Diff -> Int
numFilesAffected = length . filediffs

numMatchingLines :: (FileChange -> Bool) -> (ListDiff Line -> [b]) -> Diff -> Int
numMatchingLines isMatchingChange matchingLines
    = sum
    . map (length . matchingLines . listDiff)
    . filter isMatchingChange
    . map change
    . filediffs

-- | Number of lines added in a diff.
numAddedLines :: Diff -> Int
numAddedLines = numMatchingLines (not . isDel) adds

-- | Number of lines deleted in a diff.
numDeletedLines :: Diff -> Int
numDeletedLines = numMatchingLines (not . isAdd) dels