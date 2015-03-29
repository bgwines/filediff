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
import Filediff.Utils ((</>), getFileDirectory)

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
                return $ Just Diff {
                    baseDir = getFileDirectory a,
                    compDir = getFileDirectory b,
                    filediffs = [filediff],
                    dirdiffs = []
                }

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

    aContents <- getDirectoryContentsSafely a
    bContents <- getDirectoryContentsSafely b

    contentsDiffs <- getDiffs $ intersect aContents bContents

    aOnlyDiffs <- getDiffs $ aContents \\ bContents

    bOnlyDiffs <- getDiffs $ bContents \\ aContents


    let allDiffs = contentsDiffs ++ aOnlyDiffs ++ bOnlyDiffs
    --putStrLn $ "allDiffs: " ++ (show allDiffs) ++ "\n"
    if null allDiffs
        then return $ Diff a b [] []
        else return
            . foldl1 (|.|)
            $ allDiffs
    where
        -- | `x` is the prefix of the "base" of the diff; `y` is the
        -- | "compare".
        getDiffs :: [FilePath] -> IO [Diff]
        getDiffs filepaths
            = filter (not . any ((==) mempty . linediff) . filediffs)
            <$> catMaybes
            <$> mapM (\fp -> diff (a </> fp) (b </> fp)) filepaths

        getDirectoryContentsSafely :: FilePath -> IO [FilePath]
        getDirectoryContentsSafely filepath = do
            exists <- D.doesDirectoryExist filepath
            if exists
                then removeDotDirs <$> D.getDirectoryContents filepath
                else return []

        removeDotDirs :: [FilePath] -> [FilePath]
        removeDotDirs = flip (\\) $ [".", ".."]

-- | /O(n)/. Apply a diff to a directory or file
applyToFile :: Filediff -> FilePath -> IO [Line]--EitherT Error IO ()
applyToFile (Filediff _ _ lineDiff) filepath = do
    result <- (applySequenceDiff lineDiff . lines) <$> readFile filepath
    --writeFile filepath (unlines result)
    return result
