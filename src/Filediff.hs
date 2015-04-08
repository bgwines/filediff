{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | The module exposing the functionality of this package
module Filediff
( -- * basic operations
  diffFiles
, diffDirectories
, diffDirectoriesWithIgnoredSubdirs
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
    , getDirectoryContentsRecursiveSafe
    , isPrefix)

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
    case (aExists, bExists) of
        (False, False) -> return $ Filediff a b mempty
        (False, True ) -> addCase
        (True , False) -> delCase
        (True , True ) -> modCase
    where
        addCase :: IO Filediff
        addCase = do
            let aLines = []
            bLines <- T.lines <$> TIO.readFile b
            return Filediff
                { base = a
                , comp = b
                , change = Add $ diffSequences aLines bLines }

        delCase :: IO Filediff
        delCase = do
            aLines <- T.lines <$> TIO.readFile a
            let bLines = []
            return Filediff
                { base = a
                , comp = b
                , change = Del $ diffSequences aLines bLines }

        modCase :: IO Filediff
        modCase = do
            aLines <- T.lines <$> TIO.readFile a
            bLines <- T.lines <$> TIO.readFile b
            return Filediff
                { base = a
                , comp = b
                , change = Mod $ diffSequences aLines bLines }

-- | Compute the difference between the two directories (more
--   specifically, the minimal number of changes to make to transform the
--   directory residing at the location specified by the first
--   parameter into the second). Throws an exception if either or both of
--   the parameters point to a file, not a directory.
diffDirectories :: FilePath -> FilePath -> IO Diff
diffDirectories a b = diffDirectoriesWithIgnoredSubdirs a b [] []

-- | Diff two directories, ignoring some subdirectories. The first
-- `[FilePath]` parameter refers to the first `FilePath` parameter,
-- and same for the second, respectively.
diffDirectoriesWithIgnoredSubdirs :: FilePath -> FilePath -> [FilePath] -> [FilePath] -> IO Diff
diffDirectoriesWithIgnoredSubdirs a b aToIgnore bToIgnore = do
    aIsFile <- D.doesFileExist a
    bIsFile <- D.doesFileExist b
    when (aIsFile || bIsFile) $ error $ "One or both of " ++ a ++ " and " ++ b ++ "is not a directory, but a file."

    aContents' <- getDirectoryContentsRecursiveSafe a
    bContents' <- getDirectoryContentsRecursiveSafe b
    let aContents = filter (not . shouldIgnore aToIgnore) aContents'
    let bContents = filter (not . shouldIgnore bToIgnore) bContents'

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
        isIdentityFileDiff = (==) mempty . change

        removeFirstPathComponentFromDiff :: Filediff -> Filediff
        removeFirstPathComponentFromDiff (Filediff base comp change) =
            Filediff
                (removeFirstPathComponent base)
                (removeFirstPathComponent comp)
                change

        shouldIgnore :: [FilePath] -> FilePath -> Bool
        shouldIgnore toIgnore filepath = any (flip isPrefix $ filepath) toIgnore

-- | /O(n)/. Apply a diff to a directory or file
applyToFile :: Filediff -> FilePath -> IO [Line]--EitherT Error IO ()
applyToFile (Filediff _ _ change) filepath = do
    case change of
        Del _       -> delCase
        Mod seqdiff -> modCase seqdiff
        Add seqdiff -> addCase seqdiff
    where
        delCase :: IO [Line]
        delCase = D.removeFile filepath >> return []

        addCase :: SeqDiff Line -> IO [Line]
        addCase seqdiff = createFileWithContents filepath "" >> modCase seqdiff

        modCase :: SeqDiff Line -> IO [Line]
        modCase seqdiff = do
            -- Data.Text.IO.readFile is strict, which is what we
            -- need, here (because of the write right after)
            file <- TIO.readFile filepath 
            let result = applySequenceDiff seqdiff . T.lines $ file
            TIO.writeFile filepath (safeInit . T.unlines $ result) -- init for trailing \n
            return result

        safeInit :: T.Text -> T.Text
        safeInit x = if T.null x then x else T.init x

-- | `True` upon success; `False` upon failure
applyToDirectory :: Diff -> FilePath -> IO ()
applyToDirectory (Diff filediffs) filepath = mapM_ apply filediffs
    where
        apply :: Filediff -> IO [Line]
        apply diff@(Filediff base compare _)
            = applyToFile diff (filepath </> base)
