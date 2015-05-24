{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | The module exposing the functionality of this package
module Filediff
( -- * lists
  diffLists
, applyListDiff

  -- * files
, diffFiles
, applyFileDiff

  -- * directories
, diffDirectories
, diffDirectoriesWithIgnoredSubdirs
, applyDirectoryDiff
) where

import Debug.Trace
import qualified Data.HashMap as HMap

import Control.Concurrent (forkIO)
import Control.Concurrent.Thread as Thread (Result(..), result)
import Control.Concurrent.Thread.Group as ThreadGroup (new, forkIO, wait)

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.MemoCombinators as Memo

import Data.Either.Combinators

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

-- | /O(n)/. Apply a diff to a file. Returns the fail state if
-- application fails. For more on how diff application can fail,
-- see 'applyListDiff'.
applyFileDiff :: Filediff -> FilePath -> EitherT Error IO [Line]
applyFileDiff (Filediff _ _ change) filepath = do
    case change of
        Del _       -> delCase
        Mod listdiff -> modCase listdiff
        Add listdiff -> addCase listdiff
    where
        delCase :: EitherT Error IO [Line]
        delCase = liftIO (D.removeFile filepath) >> right []

        addCase :: ListDiff Line -> EitherT Error IO [Line]
        addCase listDiff = liftIO (createFileWithContents filepath "") >> modCase listDiff

        modCase :: ListDiff Line -> EitherT Error IO [Line]
        modCase listDiff = do
            -- Data.Text.IO.readFile is strict, which is what we
            -- need, here (because of the write right after)
            file <- liftIO (TIO.readFile filepath)
            result <- hoistEither (applyListDiff listDiff . T.lines $ file)
            liftIO (TIO.writeFile filepath (safeInit . T.unlines $ result))-- `init` for trailing \n
            right result

        safeInit :: T.Text -> T.Text
        safeInit x = if T.null x then x else T.init x

-- | Applies a `Diff` to a directory. Returns the fail state if
-- any file application fails, but because of the parallelism in the
-- implementation, all file diffs will be attempted to be applied, so
-- if this fails, your directory will be left in an inconsistent state.
-- For more on how diff application can fail, see 'applyListDiff'.
applyDirectoryDiff :: Diff -> FilePath -> EitherT Error IO [[Line]]
applyDirectoryDiff (Diff filediffs) filepath
    = EitherT
    . fmap sequence
    $ mapMParallelWaitForAll (runEitherT . apply) filediffs >>= mapM Thread.result
    where
        apply :: Filediff -> EitherT Error IO [Line]
        apply diff@(Filediff base compare _)
            = applyFileDiff diff (filepath </> base)

-- | forks the given 'IO' action for each element in the given list,
-- but waits for all to finish before returning.
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
diffLists :: forall a. (Eq a) => [a] -> [a] -> ListDiff a
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
        getProgressiveIndicesToAdd :: [a] -> [a] -> [(Int, a)]
        getProgressiveIndicesToAdd sub super =
            map (\i -> (i, super !! i)) $ nonSubsequenceIndices sub super

-- | Applies a list diff. For example,
--
--       > ListDiff {dels = [(2,'c'),(3,'d'),(5,'f'),(6,'g')], adds = [(0,'w'),(3,'x'),(4,'y'),(5,'z')]}
--       > λ applyListDiff it "abcdefg"
--       > Right "wabxyze"
--
-- Returns a fail state if the diff cannot be applied. This can happen
-- for two reasons: first, the diff calls for a deletion at an index
-- but the element at that index doesn't match the element believed by
-- the to be diff at that index. Second, it can happen if the diff calls
-- for an element to be added at an index too large for the given input.
-- Here are respective examples of inputs that would trigger this case:
--
--     > let base = "abcdefg"
--     > let faultyBase = "ab*defg"
--     > let comp = "wabxyze"
--     > let listDiff = F.diffLists base comp
--     > F.applyListDiff listDiff faultyBase -- fails
--
-- and
--
--     > let base = "abcdefg"
--     > let faultyBase = "abcde"
--     > let comp = "wabxyzefgq"
--     > let listDiff = F.diffLists base comp
--     > F.applyListDiff listDiff faultyBase -- fails
applyListDiff :: forall a. (Eq a) => ListDiff a -> [a] -> Either Error [a]
applyListDiff l@(ListDiff dels adds) list =
    removeAtIndices dels list >>= insertAtProgressiveIndices adds

-- | Best explained by example:
--
--       > λ insertAtProgressiveIndices [(1,'a'),(3,'b')] "def"
--       > Just "daebf"
insertAtProgressiveIndices :: [(Int, a)] -> [a] -> Either Error [a]
insertAtProgressiveIndices = insertAtProgressiveIndices' 0

insertAtProgressiveIndices' :: Int -> [(Int, a)] -> [a] -> Either Error [a]
insertAtProgressiveIndices' _ [] dest = Right dest
insertAtProgressiveIndices' curr src@((i,s):src') dest =
    if i == curr
        then (:) s <$> insertAtProgressiveIndices' (succ curr) src' dest
        else case dest of
            (d:dest') -> (:) d <$> insertAtProgressiveIndices' (succ curr) src dest'
            [] -> Left "Fatal: couldn't apply list diff (application requires inserting at an index larger than the length of the list to which to apply the diff)."

-- all functions below are not exposed

-- don't hit the memotable if not necessary
-- | A wrapper around `longestCommonSubsequence`. It gives a bit of a
-- performance boost; it avoids hitting the memo table to an extent
-- (exactly how much depends on the arguments).
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

        -- xs = abcd***efg
        -- ys = abcd???????efg
        -- getMiddle xs == ****
        -- getMiddle ys = ??????
        getMiddle :: [a] -> [a]
        getMiddle elems = take (length elems - length commonPrefix - length commonSuffix) . drop (length commonPrefix) $ elems

-- | Compute the longest common (potentially noncontiguous) subsequence
--   between two sequences. Element type is fixed because memoization
--   requires a static type.
longestCommonSubsequence :: forall a. (Eq a) => [a] -> [a] -> [a]
longestCommonSubsequence xs ys = longestCommonSubsequence' 0 0
    where
        -- TODO: UArray?
        xs' :: HMap.Map Int a
        xs' = foldl update HMap.empty (zip [0..] xs)

        ys' :: HMap.Map Int a
        ys' = foldl update HMap.empty (zip [0..] ys)

        update :: HMap.Map Int a -> (Int, a) -> HMap.Map Int a
        update hmap (i, a) = HMap.insert i a hmap

        xsLength :: Int
        xsLength = length xs

        ysLength :: Int
        ysLength = length ys

        longestCommonSubsequence' :: Int -> Int -> [a]
        longestCommonSubsequence' = Memo.memo2 Memo.integral Memo.integral longestCommonSubsequence''

        longestCommonSubsequence'' :: Int -> Int -> [a]
        longestCommonSubsequence'' i j
            | i == xsLength = []
            | j == ysLength = []
            | x == y = x : longestCommonSubsequence' (i + 1) (j + 1) -- WLOG
            | length caseX > length caseY = caseX
            | otherwise = caseY
            where
                x :: a
                x = xs' HMap.! i

                y :: a
                y = ys' HMap.! j

                caseX :: [a]
                caseX = longestCommonSubsequence' (i + 1) j

                caseY :: [a]
                caseY = longestCommonSubsequence' i (j + 1)

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
--   and indices must all exist.
removeAtIndices :: forall a. (Eq a) => [(Int, a)] -> [a] -> Either Error [a]
removeAtIndices dels list = if not matches
    then Left "Fatal: couldn't apply list diff (application requires removing an element where the diff calls for a different element residing at that index)."
    else Right (removeAtIndices' 0 (map fst dels) list)
    where
        matches :: Bool
        matches = all (< length list) (map fst dels)
            && all (\(i, ch) -> (list !! i) == ch) dels

        removeAtIndices' :: Int -> [Int] -> [a] -> [a]
        removeAtIndices' _ [] xs = xs
        removeAtIndices' curr (i:is) (x:xs) =
            if curr == i
                then     removeAtIndices' (succ curr) is xs
                else x : removeAtIndices' (succ curr) (i:is) xs
