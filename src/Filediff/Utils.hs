{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions not to be exposed by `Filediff`
module Filediff.Utils
( -- * filesystem operations
  (</>)
, getFileDirectory
, removeDotDirs
, removeFirstPathComponent
, getDirectoryContentsRecursiveSafe

-- * list operations
, dropUntil
) where

import Data.List ((\\))

import Control.Monad
import Control.Applicative

import qualified System.IO as IO
import qualified System.Directory as D

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
    = (?:) ((/=) "") "."
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

-- returns relative to `directory`
getDirectoryContentsRecursiveSafe :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveSafe directory = do
    contents <- getDirectoryContentsRecursiveSafe' directory
    return . map removeFirstPathComponent $ contents

getDirectoryContentsRecursiveSafe' :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveSafe' directory = do
    exists <- D.doesDirectoryExist directory
    if not exists
        then return []
        else do
            relativeContents <- removeDotDirs <$> D.getDirectoryContents directory
            let contents = map ((</>) directory) relativeContents

            files <- filterM D.doesFileExist contents
            directories <- filterM D.doesDirectoryExist contents

            recFiles <- concat <$> mapM getDirectoryContentsRecursiveSafe' directories

            return $ files ++ recFiles

-- * list operations

-- | Drops elements from the given list until the predicate function
-- | returns `True` (returned list includes element that passes test)
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs) =
    if f x
        then (x:xs)
        else dropUntil f xs
