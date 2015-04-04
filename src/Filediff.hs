{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | The module exposing the functionality of this package
module Filediff
( -- * basic operations
  diffFiles
, diffDirectories
, applyToFile
, applyToDirectory
) where

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- function imports

import Data.Maybe (isJust, fromJust, catMaybes)

import Data.List ((\\), intersect)

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- Filediff imports

import Filediff.Types
import Filediff.Sequence (SeqDiff(..), diffSequences, applySequenceDiff)
import Filediff.Utils
    ( (</>)
    , (<.>)
    , getFileDirectory
    , removeDotDirs
    , createFileWithContents
    , dropUntil
    , removeFirstPathComponent 
    , getDirectoryContentsRecursiveSafe )

-- * basic operations

-- | /O(mn)/. Compute the difference between the two files (more
--   specifically, the minimal number of changes to make to transform the
--   file residing at the location specified by the first
--   parameter into the second). Throws an exception if either or both of
--   the parameters point to a directory, not a file.
--  
--   Files are allowed to not exist at either or both of the parameters.
diffFiles :: FilePath -> FilePath -> IO Filediff
diffFiles a b = do
    aIsDir <- D.doesDirectoryExist a
    bIsDir <- D.doesDirectoryExist b
    when (aIsDir || bIsDir) $ error $ "One or both of " ++ a ++ " and " ++ b ++ "is not a file, but a directory."

    aExists <- D.doesFileExist a
    bExists <- D.doesFileExist b
    aLines <- if aExists then T.lines <$> TIO.readFile a else return []
    bLines <- if bExists then T.lines <$> TIO.readFile b else return []
    let linediff = diffSequences aLines bLines
    return Filediff
        { base = a
        , comp = b
        , linediff = linediff }

-- | Compute the difference between the two directories (more
--   specifically, the minimal number of changes to make to transform the
--   directory residing at the location specified by the first
--   parameter into the second). Throws an exception if either or both of
--   the parameters point to a file, not a directory.
diffDirectories :: FilePath -> FilePath -> IO Diff
diffDirectories a b = do
    aIsFile <- D.doesFileExist a
    bIsFile <- D.doesFileExist b
    when (aIsFile || bIsFile) $ error $ "One or both of " ++ a ++ " and " ++ b ++ "is not a directory, but a file."

    aContents <- getDirectoryContentsRecursiveSafe a
    bContents <- getDirectoryContentsRecursiveSafe b

    intersectionDiffs <- getDiffs $ intersect aContents bContents

    aOnlyDiffs <- getDiffs $ aContents \\ bContents

    bOnlyDiffs <- getDiffs $ bContents \\ aContents

    let allDiffs = map removeFirstPathComponentFromDiff $ intersectionDiffs ++ aOnlyDiffs ++ bOnlyDiffs

    return $ Diff allDiffs
    where
        -- | `x` is the prefix of the "base" of the diff; `y` is the
        -- | "compare".
        getDiffs :: [FilePath] -> IO [Filediff]
        getDiffs filepaths
            = filter (not . isIdentityFileDiff)
            <$> mapM (\fp -> diffFiles (a </> fp) (b </> fp)) filepaths

        isIdentityFileDiff :: Filediff -> Bool
        isIdentityFileDiff = (==) mempty . linediff

        removeFirstPathComponentFromDiff :: Filediff -> Filediff
        removeFirstPathComponentFromDiff (Filediff base comp seqdiff)
            = Filediff
                (removeFirstPathComponent base)
                (removeFirstPathComponent comp)
                seqdiff

-- | /O(n)/. Apply a diff to a directory or file
applyToFile :: Filediff -> FilePath -> IO [Line]--EitherT Error IO ()
applyToFile (Filediff _ _ linediff) filepath = do
    exists <- D.doesFileExist filepath
    when (not exists) $ createFileWithContents filepath ""

    -- Data.Text.IO.readFile is strict, which is what we
    -- need, here (because of the write right after)
    fileContents <- TIO.readFile filepath 
    let result = (applySequenceDiff linediff . T.lines) fileContents
    TIO.writeFile filepath (T.unlines result)
    return result

-- | `True` upon success; `False` upon failure
applyToDirectory :: Diff -> FilePath -> IO ()
applyToDirectory (Diff filediffs) filepath = mapM_ apply filediffs
    where
        apply :: Filediff -> IO [Line]
        apply diff@(Filediff base compare linediff)
            = applyToFile diff (filepath </> base)
