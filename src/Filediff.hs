{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | The module exposing the functionality of this package
module Filediff
( -- * lists
  diffLists
, applyListDiff

  -- * files
, diffFiles
, applyToFile

  -- * directories
, diffDirectories
, diffDirectoriesWithIgnoredSubdirs
, applyToDirectory
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Thread as Thread (Result(..))
import Control.Concurrent.Thread.Group as ThreadGroup (new, forkIO, wait)

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.MemoCombinators.Class (MemoTable, table)
import qualified Data.MemoCombinators as Memo

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
import Filediff.Utils
    ( (</>)
    , (<.>)
    , getFileDirectory
    , removeDotDirs
    , createFileWithContents
    , dropUntil
    , removeFirstPathComponent
    , removePathComponents
    , getDirectoryContentsRecursiveSafe
    , isPrefix
    , dropPrefix
    , dropInitialSlash
    , dropTrailingSlash )

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
                , change = Add $ diffLists aLines bLines }

        delCase :: IO Filediff
        delCase = do
            aLines <- T.lines <$> TIO.readFile a
            let bLines = []
            return Filediff
                { base = a
                , comp = b
                , change = Del $ diffLists aLines bLines }

        modCase :: IO Filediff
        modCase = do
            aLines <- T.lines <$> TIO.readFile a
            bLines <- T.lines <$> TIO.readFile b
            return Filediff
                { base = a
                , comp = b
                , change = Mod $ diffLists aLines bLines }

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
diffDirectoriesWithIgnoredSubdirs a' b' aToIgnore bToIgnore = do
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

    let allDiffs = map makeRelative $ intersectionDiffs ++ aOnlyDiffs ++ bOnlyDiffs

    return $ Diff allDiffs
    where
        a :: FilePath
        a = dropTrailingSlash a'

        b :: FilePath
        b = dropTrailingSlash b'

        -- | `x` is the prefix of the "base" of the diff; `y` is the
        -- | "compare".
        getDiffs :: [FilePath] -> IO [Filediff]
        getDiffs filepaths
            = filter (not . isIdentityFileDiff)
            <$> mapM (\fp -> diffFiles (a </> fp) (b </> fp)) filepaths

        isIdentityFileDiff :: Filediff -> Bool
        isIdentityFileDiff = (==) mempty . change

        makeRelative :: Filediff -> Filediff
        makeRelative (Filediff base comp change) =
            Filediff
                (dropInitialSlash $ dropPrefix a base)
                (dropInitialSlash $ dropPrefix b comp)
                change

        shouldIgnore :: [FilePath] -> FilePath -> Bool
        shouldIgnore toIgnore filepath = any (flip isPrefix $ filepath) toIgnore

-- | /O(n)/. Apply a diff to a file. Throws an exception if the
--   application fails.
applyToFile :: Filediff -> FilePath -> IO [Line]--EitherT Error IO ()
applyToFile (Filediff _ _ change) filepath = do
    case change of
        Del _       -> delCase
        Mod listdiff -> modCase listdiff
        Add listdiff -> addCase listdiff
    where
        delCase :: IO [Line]
        delCase = D.removeFile filepath >> return []

        addCase :: ListDiff Line -> IO [Line]
        addCase listDiff = createFileWithContents filepath "" >> modCase listDiff

        modCase :: ListDiff Line -> IO [Line]
        modCase listDiff = do
            -- Data.Text.IO.readFile is strict, which is what we
            -- need, here (because of the write right after)
            file <- TIO.readFile filepath 
            let result = applyListDiff listDiff . T.lines $ file
            TIO.writeFile filepath (safeInit . T.unlines $ result) -- `init` for trailing \n
            return result

        safeInit :: T.Text -> T.Text
        safeInit x = if T.null x then x else T.init x

-- | Applies a `Diff` to a directory. Throws an exception if the
--   application fails.
applyToDirectory :: Diff -> FilePath -> IO ()
applyToDirectory (Diff filediffs) filepath
    = void $ mapMParallelWaitForAll (void . apply) filediffs
    where
        apply :: Filediff -> IO [Line]
        apply diff@(Filediff base compare _)
            = applyToFile diff (filepath </> base)

mapMParallelWaitForAll :: (a -> IO b) -> [a] -> IO [Thread.Result b]
mapMParallelWaitForAll f list = do
    group <- ThreadGroup.new
    ioResults <- mapM (fmap snd . ThreadGroup.forkIO group . f) list
    ThreadGroup.wait group
    sequence ioResults

-- | Computes the minimal number of additions and deletions needed to
--   transform the first parameter into the second.
--
--       > λ diffLists "abcdefg" "wabxyze"
--       > ListDiff {dels = [(2,'c'),(3,'d'),(5,'f'),(6,'g')], adds = [(0,'w'),(3,'x'),(4,'y'),(5,'z')]}
diffLists :: forall a. (Eq a, MemoTable a) => [a] -> [a] -> ListDiff a
diffLists a b = ListDiff
    (map (\i -> (i, a !! i)) $ nonSubsequenceIndices common a)
    (getProgressiveIndicesToAdd common b)
    where
        common :: [a]
        common = longestCommonSubsequenceWrapper a b

        -- | > λ add
        --   > [(0,"w"),(3,"x"),(4,"y")]
        --   > λ common
        --   > ["a","b","e"]
        getProgressiveIndicesToAdd :: (Eq a) => [a] -> [a] -> [(Int, a)]
        getProgressiveIndicesToAdd sub super =
            map (\i -> (i, super !! i)) $ nonSubsequenceIndices sub super

-- |     > λ diffLists "abcdefg" "wabxyze"
--       > ListDiff {dels = [(2,'c'),(3,'d'),(5,'f'),(6,'g')], adds = [(0,'w'),(3,'x'),(4,'y'),(5,'z')]}
--       > λ applyListDiff it "abcdefg"
--       > "wabxyze"
--
-- Throws an exception if the diff can't be applied.
applyListDiff :: forall a. (Eq a) => ListDiff a -> [a] -> [a]
applyListDiff (ListDiff dels adds)
    = insertAtProgressiveIndices adds . removeAtIndices dels
    where
        -- | Best explained by example:
        -- |
        -- |     > λ insertAtProgressiveIndices [(1,'a'),(3,'b')] "def"
        -- |     > "daebf"
        insertAtProgressiveIndices :: [(Int, a)] -> [a] -> [a]
        insertAtProgressiveIndices = insertAtProgressiveIndices' 0

        insertAtProgressiveIndices' :: Int -> [(Int, a)] -> [a] -> [a]
        insertAtProgressiveIndices' _ [] dest = dest
        insertAtProgressiveIndices' curr src@((i,s):src') [] =
            s : insertAtProgressiveIndices' (succ curr) src' []
        insertAtProgressiveIndices' curr src@((i,s):src') dest@(d:dest') =
            if i == curr
                then s : insertAtProgressiveIndices' (succ curr) src' dest
                else d : insertAtProgressiveIndices' (succ curr) src dest'

-- all functions below are not exposed

-- don't hit the memotable if not necessary
longestCommonSubsequenceWrapper :: forall a. (Eq a) => [a] -> [a] -> [a]
longestCommonSubsequenceWrapper xs ys =
    if xs == ys
        then xs -- (WLOG) don't want to return xs ++ xs
        else commonPrefix
            ++ longestCommonSubsequence (getMiddle xs) (getMiddle ys)
            ++ commonSuffix
    where
        commonPrefix :: [a]
        commonPrefix = getCommonPrefix xs ys

        -- drop (length commonPrefix) to prevent the "abc" vs. "abc*abc" case
        commonSuffix :: [a]
        commonSuffix = reverse
            (getCommonPrefix
                (reverse (drop (length commonPrefix) xs))
                (reverse (drop (length commonPrefix) ys)))

        getCommonPrefix :: [a] -> [a] -> [a]
        getCommonPrefix as bs = map fst . takeWhile (uncurry (==)) $ zip as bs

        getMiddle :: [a] -> [a]
        getMiddle elems = take (length elems - length commonPrefix - length commonSuffix) . drop (length commonPrefix) $ elems

-- | Compute the longest common (potentially noncontiguous) subsequence
--   between two sequences. Element type is fixed because memoization
--   requires a static type.
longestCommonSubsequence :: forall a. (Eq a) => [a] -> [a] -> [a]
longestCommonSubsequence xs ys = longestCommonSubsequence' xs ys 0 0

-- optimization: hash lines
-- | Compute the longest common (potentially noncontiguous) subsequence
--   between two sequences. Element type is fixed because memoization
--   requires a static type.
longestCommonSubsequence' :: forall a. (Eq a) =>
                                [a] -> [a] -> Int -> Int -> [a]
longestCommonSubsequence' xs ys i j
    = (Memo.memo2 Memo.integral Memo.integral
            (longestCommonSubsequence'' xs ys)) i j
    where
        longestCommonSubsequence'' :: [a] -> [a] -> Int -> Int -> [a]
        longestCommonSubsequence'' [] _ _ _ = []
        longestCommonSubsequence'' _ [] _ _ = []
        longestCommonSubsequence'' (x:xs) (y:ys) i j =
            if x == y
                then x : (longestCommonSubsequence' xs ys (i + 1) (j + 1)) -- WLOG
                else if (length caseX) > (length caseY)
                    then caseX
                    else caseY
            where
                caseX :: [a]
                caseX = longestCommonSubsequence' xs (y:ys) (i+1) j

                caseY :: [a]
                caseY = longestCommonSubsequence' (x:xs) ys i (j+1)

-- | When `sub` is a (not necessarily contiguous) subsequence of `super`,
--   get the index at which each element of `sub` appears. E.g.
--  
--       > λ subsequenceIndices "abe" "abcdefg"
--       > [0,1,4]
subsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
subsequenceIndices [] _ = []
subsequenceIndices _ [] = error "`sub` was not a subsequence of `super`"
subsequenceIndices sub@(a:sub') super@(b:super') =
    if a == b
        then 0 : map succ (subsequenceIndices sub' super')
        else     map succ (subsequenceIndices sub super')

-- | When `sub` is a (not necessarily contiguous) subsequence of `super`,
--   get the indices at which elements of `sub` do *not* appear. E.g.
--  
--       > λ nonSubsequenceIndices "abe" "abcdefg"
--       > [2,3,5,6]
nonSubsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
nonSubsequenceIndices sub super =
    [0..(length super - 1)] \\ (subsequenceIndices sub super)

-- | /O(n)/. `indices` parameter *must* be sorted in increasing order,
--   and indices must all exist. Throws an exception if the provided
--   list doesn't have those elements at those indices.
removeAtIndices :: forall a. (Eq a) => [(Int, a)] -> [a] -> [a]
removeAtIndices dels list = if not matches
    then error $ "Fatal: can't apply this diff to this list."
    else removeAtIndices' 0 (map fst dels) list
    where
        matches :: Bool
        matches = all (\(i, ch) -> (list !! i) == ch) dels

        removeAtIndices' :: Int -> [Int] -> [a] -> [a]
        removeAtIndices' _ [] xs = xs
        removeAtIndices' curr (i:is) (x:xs) =
            if curr == i
                then     removeAtIndices' (succ curr) is xs
                else x : removeAtIndices' (succ curr) (i:is) xs
