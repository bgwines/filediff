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
, removePathComponents
, getDirectoryContentsRecursiveSafe

  -- * file path formatting
, dropInitialSlash
, dropTrailingSlash

  -- * list operations
, dropUntil
, isPrefix
, dropPrefix
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

-- | Removes the k oldest ancestors from a path component, e.g.
-- |
-- |     > removePathComponents 2 "a/b/c"
-- |     "c"
removePathComponents :: Int -> FilePath -> FilePath
removePathComponents k
    = last
    . take k
    . iterate removeFirstPathComponent

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

    return . map (removePathComponents $ numPathComponents + 1) $ contents

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

-- * file path formatting

-- | If the parameter has a '/' as its first character, drop it.
dropInitialSlash :: String -> String
dropInitialSlash ('/':s) = s
dropInitialSlash s = s

-- | If the parameter has a '/' as its last character, drop it.
dropTrailingSlash :: String -> String
dropTrailingSlash [] = []
dropTrailingSlash s = if last s == '/'
    then init s
    else s

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

-- | assumes `a` is a prefix of `b`; errors if false
dropPrefix :: (Eq a) => [a] -> [a] -> [a]
dropPrefix [] bs = bs
dropPrefix (a:as) (b:bs)
    | a /= b = error "not a prefix"
    | otherwise = dropPrefix as bs
