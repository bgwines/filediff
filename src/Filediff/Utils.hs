{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions not to be exposed by `Filediff`
module Filediff.Utils
( -- * directory operations
  (</>)
, getFileDirectory
) where

-- | Concatenates two filepaths, for example:
-- |
-- |     > "a/b" </> "c"
-- |     "a/b/c"
-- |
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- | Ternary operator: if the predicate function evalues to `True`
-- | , take the second argument; otherwise, the first.
(?:) :: (a -> Bool) -> a -> a -> a
(?:) f a' a = if f a then a else a'

-- |     > getFileDirectory "a/b/c/d/e.txt"
-- |     "a/b/c/d/"
-- |
-- |     > getFileDirectory "a/b/c/d/"
-- |     "a/b/c/d/"
-- |
-- |     > getFiledirectory "file.txt"
-- |     "."
getFileDirectory :: FilePath -> FilePath
getFileDirectory filepath
    = (?:) ((/=) "") "." -- get current directory, relative to repo root? yeah...
    . reverse
    . dropWhile ((/=) '/')
    . reverse
    $ filepath
