{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions not to be exposed by `Filediff`
module Filediff.Utils
( -- * directory operations
  (</>)
, getFileDirectory
, removeDotDirs
, removeFirstPathComponent

-- * list operations
, dropUntil
) where

import Data.List ((\\))

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

-- | Takes a list of filepaths, and removes "." and ".." from it.
removeDotDirs :: [FilePath] -> [FilePath]
removeDotDirs = flip (\\) $ [".", ".."]

-- TODO: safe tail?
-- | Removes the oldest ancestor from a path component, e.g.
-- |
-- |     > removeFirstPathComponent "a/b/c"
-- |     "b/c"
removeFirstPathComponent :: FilePath -> FilePath
removeFirstPathComponent = tail . dropUntil ((==) '/')

-- * list operations

-- | Drops elements from the given list until the predicate function
-- | returns `True` (returned list includes element that passes test)
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs) =
    if f x
        then (x:xs)
        else dropUntil f xs
