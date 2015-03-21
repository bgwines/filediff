module Filediff.Types
( Diff(..)
, Dels
, Adds
, Line
, Error
, Index
) where

-- | The basic data type for a difference between two files
data Diff = Diff Dels Adds deriving (Eq, Show)

-- | How we represent deletions: a list of indices at which to remove
-- | elements. Implementation constant: will always be sorted.
type Dels = [Index]

-- | How additions are represented: each line to add comes with the
-- | index at which it will eventually reside.
type Adds = [(Index, Line)]

-- | Data type for a line
type Line = String

-- | Basic error type
type Error = String

-- | List index
type Index = Int