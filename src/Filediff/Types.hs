-- | Data types used by `Filediff`
module Filediff.Types
( Diff(..)
, Line
, Error
, Index
) where

-- | The basic data type for a difference between two files. The
-- | `FilePath` is the "old" file in the old-new comparison, and
-- | is the file to which the patch will be applied. Deletions: a list
-- | of indices at which to remove elements. Additions: each line to add
-- | comes with the index at which it will eventually reside.
data Diff = Diff {
    targetFile :: FilePath,
    dels :: [Index],
    adds :: [(Index, Line)]
} deriving (Eq, Show)

-- | Data type for a line
type Line = String

-- | Basic error type
type Error = String

-- | List index
type Index = Int