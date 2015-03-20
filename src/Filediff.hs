module Filediff
( -- * data types
  Diff(..)

-- * basic operations
, diffFiles
, diffDirectories
, apply

-- * additional operations
, compose
) where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- * data types

-- | The basic data type for a difference between two files
data Diff = Diff [Linechange]

-- | A data type representing a change to a line (either an addition
-- | or a deletion)
data Linechange
    = Add Line
    | Del Line
    -- mod?

-- | Data type for a line
type Line = String

-- | Basic error type
type Error = String

-- * basic operations

-- | Compute the difference between the two files (more
-- | specifically, the minimal number of changes to make to transform the
-- | file residing at the location specified by the first
-- | parameter into the second). Returns a fail state if either or both of
-- | the parametres point to a directory, not a file.
diffFiles :: FilePath -> FilePath -> EitherT Error IO Diff
diffFiles f1 f2 = error "not yet implemented"

-- | Compute the difference between the two directories (more
-- | specifically, the minimal number of changes to make to transform the
-- | directory residing at the location specified by the first
-- | parameter into the second). Returns a fail state if either or both of
-- | the parametres point to a file, not a directory.
diffDirectories :: FilePath -> FilePath -> IO Diff
diffDirectories d1 d2 = error "not yet implemented"

-- | Apply a diff to a directory or file
apply :: Diff -> FilePath -> EitherT Error IO ()
apply d filepath = error "not yet implemented"

-- * additional operatinos

-- | Composition may fail
compose :: Diff -> Diff -> Maybe Diff
compose a b = error "not yet implemented"
