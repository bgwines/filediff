{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper functions not to be exposed by `Filediff`
module Filediff.Utils
( -- * filesystem operations
  (</>)
, (<.>)
, getFileDirectory
, removeDotDirs
, createFileWithContents
, removeFirstPathComponent
, getDirectoryContentsRecursiveSafe

-- * list operations
, dropUntil
, isPrefix
) where

import Data.List ((\\), inits)

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

-- | Function composition, but where the inner function's returnvalue
-- | is inside a functor.
(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> (a -> f c)
f <.> g = \a -> f <$> (g a)

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

-- | Creates a file at the specified path with the specified contents.
-- | If intermediate directories do not exist, it creates them.
createFileWithContents :: FilePath -> String -> IO ()
createFileWithContents filepath contents = do
    let intermediateDirs = filter ((==) '/' . last) . tail . inits $ filepath
    dirsToCreate <- filterM (not <.> D.doesDirectoryExist) intermediateDirs
    mapM_ D.createDirectory dirsToCreate

    handle <- IO.openFile filepath IO.WriteMode
    IO.hPutStr handle contents
    IO.hClose handle

-- TODO: safe tail?
-- | Removes the oldest ancestor from a path component, e.g.
-- |
-- |     > removeFirstPathComponent "a/b/c"
-- |     "b/c"
removeFirstPathComponent :: FilePath -> FilePath
removeFirstPathComponent path =
    if null . filter ((==) '/') $ path
         then error "path without '/' in it"
         else tail . dropUntil ((==) '/') $ path

-- | Gets paths to all files in or in subdirectories of the
-- | specified directory. Returned paths are relative to the
-- | given directory.
getDirectoryContentsRecursiveSafe :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveSafe directory = do
    contents <- getDirectoryContentsRecursiveSafe' directory

    let directoryWithTrailingSlash = if last directory == '/'
        then directory
        else directory </> ""
    let numPathComponents = length . filter ((==) '/') $ directoryWithTrailingSlash
    let removePathComponents = last . take (numPathComponents + 1) . iterate removeFirstPathComponent

    return . map removePathComponents $ contents

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

-- | (intended to be used infix)
isPrefix :: (Eq a) => [a] -> [a] -> Bool
a `isPrefix` b = (==) (length a) . length . takeWhile id $ zipWith (==) a b
