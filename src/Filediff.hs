{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | The module exposing the functionality of this package
module Filediff
( -- * basic operations
  diff
, diffFiles
, diffDirectories
, applyToFile
) where

import qualified System.Directory as D

-- function imports

import Data.Maybe (isJust, fromJust, catMaybes)

import Data.List ((\\), intersect)

import Control.Applicative ((<$>), (<*>))

import Control.Monad
import Data.Monoid
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- Filediff imports

import Filediff.Types
import Filediff.Sequence (SeqDiff(..), diffSequences, applySequenceDiff)
import Filediff.Utils
    ( (</>)
    , getFileDirectory
    , removeDotDirs
    , dropUntil
    , removeFirstPathComponent )

-- * basic operations

-- | /O(mn)/. Compute the difference between the two files or directories
-- | (more specifically, the minimal number of changes to make to
-- | transform the file or directory residing at the location specified by
-- | the first parameter into the second). Returns a fail state if the
-- | parameters do not both point to a file or both to a directory.
diff :: FilePath -> FilePath -> IO (Maybe Diff)
diff a b = do
    aIsFile <- D.doesFileExist a
    bIsFile <- D.doesFileExist b

    aIsDirectory <- D.doesDirectoryExist a
    bIsDirectory <- D.doesDirectoryExist b

    case ((aIsFile, bIsFile), (aIsDirectory, bIsDirectory)) of
        ((True, True), (_, _)) -> fileCase
        ((_, _), (True, True)) -> dirCase

        ((True, False), (False, True)) -> inconsistentCase
        ((False, True), (True, False)) -> inconsistentCase

        ((True, False), (False, False)) -> fileCase
        ((False, True), (False, False)) -> fileCase

        ((False, False), (True, False)) -> dirCase
        ((False, False), (False, True)) -> dirCase

        ((False, False), (False, False)) -> neitherExistsCase

        (_, _) -> error "Programming error: undefined (dir, file)-pair case"
    where
        fileCase :: IO (Maybe Diff)
        fileCase = do
                filediff <- diffFiles a b
                return $ Just $ Diff [filediff]

        dirCase :: IO (Maybe Diff)
        dirCase = Just <$> (diffDirectories a b)

        inconsistentCase :: IO (Maybe Diff)
        inconsistentCase = return Nothing

        neitherExistsCase :: IO (Maybe Diff)
        neitherExistsCase = return Nothing

-- | /O(mn)/. Compute the difference between the two files (more
-- | specifically, the minimal number of changes to make to transform the
-- | file residing at the location specified by the first
-- | parameter into the second). Throws an exception if either or both of
-- | the parameters point to a directory, not a file.
-- |
-- | Files are allowed to not exist at either or both of the parameters.
diffFiles :: FilePath -> FilePath -> IO Filediff
diffFiles a b = do
    aIsDir <- D.doesDirectoryExist a
    bIsDir <- D.doesDirectoryExist b
    when (aIsDir || bIsDir) $ error $ "One or both of " ++ a ++ " and " ++ b ++ "is not a file, but a directory."

    aExists <- D.doesFileExist a
    bExists <- D.doesFileExist b
    aLines <- if aExists then lines <$> readFile a else return []
    bLines <- if bExists then lines <$> readFile b else return []
    let linediff = diffSequences aLines bLines
    return Filediff
        { base = a
        , comp = b
        , linediff = linediff }

-- | Compute the difference between the two directories (more
-- | specifically, the minimal number of changes to make to transform the
-- | directory residing at the location specified by the first
-- | parameter into the second). Throws an exception if either or both of
-- | the parameters point to a file, not a directory.
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
        isIdentityFileDiff (Filediff _ _ seqdiff) = seqdiff == mempty

        removeFirstPathComponentFromDiff :: Filediff -> Filediff
        removeFirstPathComponentFromDiff (Filediff base comp seqdiff)
            = Filediff
                (removeFirstPathComponent base)
                (removeFirstPathComponent comp)
                seqdiff

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

-- | /O(n)/. Apply a diff to a directory or file
applyToFile :: Filediff -> FilePath -> IO [Line]--EitherT Error IO ()
applyToFile (Filediff _ _ lineDiff) filepath = do
    result <- (applySequenceDiff lineDiff . lines) <$> readFile filepath
    --writeFile filepath (unlines result)
    return result
