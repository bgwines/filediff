{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- imports

import Test.Tasty
import Test.Tasty.HUnit

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- qualified imports

import qualified System.IO as IO
import qualified System.Directory as D

-- imported functions

import Data.List ((\\))

import System.Exit (exitSuccess)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Control.Monad ((>>=), return, when)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

import Data.Either.Combinators (isLeft, fromLeft)

import Filediff.Sequence (SeqDiff(..))
import qualified Filediff.Sequence as FSeq
import qualified Filediff as F
import qualified Filediff.Types as FTypes

-- helper functions

-- | Concatenates two filepaths, for example:
-- |
-- |     > "a/b" </> "c"
-- |     "a/b/c"
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- | Takes a list of filepaths, and removes "." and ".." from it.
removeDotDirs :: [FilePath] -> [FilePath]
removeDotDirs = flip (\\) $ [".", ".."]

-- | Removes the oldest ancestor from a path component, e.g.
-- |
-- |     > removeFirstPathComponent "a/b/c"
-- |     "b/c"
removeFirstPathComponent :: FilePath -> FilePath
removeFirstPathComponent = tail . dropUntil ((==) '/')

-- | Drops elements from the given list until the predicate function
-- | returns `True` (returned list includes element that passes test)
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs) =
    if f x
        then (x:xs)
        else dropUntil f xs

areFilesEqual :: FilePath -> FilePath -> IO Bool
areFilesEqual a b = do
    if (removeFirstPathComponent a) /= (removeFirstPathComponent b)
        then return False
        else liftM2 (==) (IO.readFile a) (IO.readFile b)

areDirectoriesEqual :: FilePath -> FilePath -> IO Bool
areDirectoriesEqual d1 d2 = do
    d1RelativeContents <- removeDotDirs <$> D.getDirectoryContents d1
    d2RelativeContents <- removeDotDirs <$> D.getDirectoryContents d2
    let d1Contents = map ((</>) d1) d1RelativeContents
    let d2Contents = map ((</>) d2) d2RelativeContents

    d1Files <- filterM D.doesFileExist d1Contents
    d2Files <- filterM D.doesFileExist d2Contents
    d1Directories <- filterM D.doesDirectoryExist d1Contents
    d2Directories <- filterM D.doesDirectoryExist d2Contents

    allFilesEqual <- and <$> zipWithM areFilesEqual d1Files d2Files

    allDirectoriesEqual <- and <$> zipWithM areDirectoriesEqual d1Directories d2Directories

    let directoryNamesEqual = and $ zipWith (==) d1Directories d2Directories

    return $ allFilesEqual && allDirectoriesEqual && directoryNamesEqual

-- set-up

-- | Runs a test in its own empty directory.
-- | Effectively, it isolates it from all other tests.
runTest :: Assertion -> Assertion
runTest t = do
    testDirectory <- getTestDirectory
    D.createDirectory testDirectory
    D.setCurrentDirectory testDirectory

    t

    D.setCurrentDirectory ".."
    D.removeDirectoryRecursive testDirectory
    where
        -- | Gives a name of a directory that is pretty much guaranteed to
        -- | exist, so it's free for creation.
        getTestDirectory :: IO FilePath
        getTestDirectory = (map formatChar . show) <$> getCurrentTime
            where
                -- | Some characters can't be in directory names.
                formatChar :: Char -> Char
                formatChar ' ' = '-'
                formatChar '.' = '-'
                formatChar ':' = '-'
                formatChar ch = ch

createFileWithContents :: FilePath -> String -> IO ()
createFileWithContents filepath contents = do
    handle <- IO.openFile filepath IO.WriteMode
    IO.hPutStr handle contents
    IO.hClose handle

-- file diff tests

testFileDiff :: Assertion
testFileDiff = do
    createFileWithContents "BASE" "a\nb\nc\nd\ne\nf\ng"
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [2,3,5,6] [(0,"w"),(3,"x"),(4,"y"),(5,"z")])
    (F.diffFiles "BASE" "COMP") >>= (flip (@?=)) expectedFilediff

testFileDiffEmptyFiles :: Assertion
testFileDiffEmptyFiles = do
    createFileWithContents "BASE" ""
    createFileWithContents "COMP" ""

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" mempty
    (F.diffFiles "BASE" "COMP") >>= (flip (@?=)) expectedFilediff

testFileDiffEmptyBase :: Assertion
testFileDiffEmptyBase = do
    createFileWithContents "BASE" ""
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [] [(0,"w"),(1,"a"),(2,"b"),(3,"x"),(4,"y"),(5,"z"),(6,"e")])
    (F.diffFiles "BASE" "COMP") >>= (flip (@?=)) expectedFilediff

testFileDiffEmptyComp :: Assertion
testFileDiffEmptyComp = do
    createFileWithContents "BASE" "a\nb\nc\nd\ne\nf\ng"
    createFileWithContents "COMP" ""

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [0,1,2,3,4,5,6] [])
    (F.diffFiles "BASE" "COMP") >>= (flip (@?=)) expectedFilediff

testNonexistentFileDiff1 :: Assertion
testNonexistentFileDiff1 = do
    createFileWithContents "BASE" "a\nb\nc\nd\ne\nf\ng"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [0,1,2,3,4,5,6] [])
    (F.diffFiles "BASE" "COMP") >>= (flip (@?=)) expectedFilediff

testNonexistentFileDiff2 :: Assertion
testNonexistentFileDiff2 = do
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [] [(0,"w"),(1,"a"),(2,"b"),(3,"x"),(4,"y"),(5,"z"),(6,"e")])
    (F.diffFiles "BASE" "COMP") >>= (flip (@?=)) expectedFilediff

testNonexistentFileDiff3 :: Assertion
testNonexistentFileDiff3 = do
    let expectedFilediff = FTypes.Filediff "BASE" "COMP" mempty
    (F.diffFiles "BASE" "COMP") >>= (flip (@?=)) expectedFilediff

testIdentityFileDiff :: Assertion
testIdentityFileDiff = do
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "COMP" "COMP" mempty
    (F.diffFiles "COMP" "COMP") >>= (flip (@?=)) expectedFilediff

-- directory diff tests

testDirDiff :: Assertion
testDirDiff = do
    D.createDirectory "a"
    D.createDirectory "b"

    D.createDirectory "a/common"
    D.createDirectory "b/common"

    D.createDirectory "a/aonly"
    D.createDirectory "b/bonly"

    createFileWithContents "a/common/x" "x\na\nx"
    createFileWithContents "b/common/x" "x\nb\nx"

    createFileWithContents "a/aonly/afile" "a\na\na"
    createFileWithContents "b/bonly/bfile" "b\nb\nb"

    actualDiff <- F.diffDirectories "a" "b"
    let expectedDiff = FTypes.Diff {
        FTypes.filediffs =
            [ FTypes.Filediff
                { FTypes.base = "common/x"
                , FTypes.comp = "common/x"
                , FTypes.linediff = (SeqDiff [1] [(1,"b")]) }
            , FTypes.Filediff
                    { FTypes.base = "aonly/afile"
                    , FTypes.comp = "aonly/afile"
                    , FTypes.linediff = (SeqDiff [0,1,2] []) }
            , FTypes.Filediff
                { FTypes.base = "bonly/bfile"
                , FTypes.comp = "bonly/bfile"
                , FTypes.linediff = (SeqDiff [] [(0,"b"),(1,"b"),(2,"b")]) } ]
        }
    actualDiff @?= expectedDiff

testDirDiffEmptyDirectories :: Assertion
testDirDiffEmptyDirectories = do
    D.createDirectory "a"
    D.createDirectory "b"

    actualDiff <- F.diffDirectories "a" "b"
    let expectedDiff = mempty
    actualDiff @?= expectedDiff

testDirDiffNoFiles :: Assertion
testDirDiffNoFiles = do
    D.createDirectory "a"
    D.createDirectory "b"

    D.createDirectory "a/common"
    D.createDirectory "b/common"

    D.createDirectory "a/aonly"
    D.createDirectory "b/bonly"

    actualDiff <- F.diffDirectories "a" "b"
    let expectedDiff = mempty
    actualDiff @?= expectedDiff

testIdentityDirDiff :: Assertion
testIdentityDirDiff = do
    createFileWithContents "BASE" "a\nb\nc\nd\ne\nf\ng"

    let expectedDiff = mempty
    (F.diffDirectories "." ".") >>= (flip (@?=)) expectedDiff

-- composition tests

testSequenceDiffComposition :: Assertion
testSequenceDiffComposition = do
    let a = ["a","b","c","d","e","f","g"]
    let b = ["w","a","b","x","y","z","e"]
    let c = ["#","x","#","#","y","e"]

    let ab = FSeq.diffSequences a b
    let bc = FSeq.diffSequences b c
    let ac = FSeq.diffSequences a c

    ab `mappend` bc @?= ac

testSequenceDiffCompositionEdgeCase1 :: Assertion
testSequenceDiffCompositionEdgeCase1 = do
    let a = ""
    let b = "bbb"
    let c = ""

    let ab = FSeq.diffSequences a b
    let bc = FSeq.diffSequences b c
    let ac = FSeq.diffSequences a c

    ab `mappend` bc @?= ac

testFileDiffComposition :: Assertion
testFileDiffComposition = do
    createFileWithContents "a" "a\nb\nc\nd\ne\nf\ng"
    createFileWithContents "b" "w\na\nb\nx\ny\nz\ne"
    createFileWithContents "c" "#\nx\n#\n#\ny\ne"

    ab <- F.diffFiles "a" "b"
    bc <- F.diffFiles "b" "c"
    ac <- F.diffFiles "a" "c"

    ab `mappend` bc @?= ac

testDirectoryDiffComposition :: Assertion
testDirectoryDiffComposition = do
    D.createDirectory "a"
    D.createDirectory "b"
    D.createDirectory "c"

    D.createDirectory "a/common"
    D.createDirectory "b/common"
    D.createDirectory "c/common"

    D.createDirectory "a/aonly"
    D.createDirectory "b/bonly"
    D.createDirectory "c/conly"

    createFileWithContents "a/common/file" "a\nb\nc\nd\ne\nf\ng"
    createFileWithContents "b/common/file" "w\na\nb\nx\ny\nz\ne"
    createFileWithContents "c/common/file" "#\nx\n#\n#\ny\ne"

    createFileWithContents "a/aonly/afile" "a\na\na"
    createFileWithContents "b/bonly/bfile" "b\nb\nb"
    createFileWithContents "c/conly/cfile" "c\nc\nc"

    ab <- F.diffDirectories "a" "b"
    bc <- F.diffDirectories "b" "c"
    ac <- F.diffDirectories "a" "c"

    ab `mappend` bc @?= ac

-- file apply tests

testFileApply :: Assertion
testFileApply = do
    let baseContents = "a\nb\nc\nd\ne\nf\ng"
    let compContents = "w\na\nb\nx\ny\nz\ne"

    createFileWithContents "BASE" baseContents
    createFileWithContents "COMP" compContents

    fileDiff <- F.diffFiles "BASE" "COMP"
    --IO.withFile "COMP" IO.WriteMode (flip IO.hPutStr $ compContents)
    applied <- F.applyToFile fileDiff "BASE"
    applied @?= (lines compContents)
    --n <- readFile "COMP"
    --o <- readFile "BASE"
    --o @?= n
    --join $ (liftM2 (@?=)) (readFile "BASE") (readFile "COMP")

-- directory apply tests

testDirApply :: Assertion
testDirApply = do
    D.createDirectory "a"
    D.createDirectory "b"

    D.createDirectory "a/common"
    D.createDirectory "b/common"

    D.createDirectory "a/aonly"
    D.createDirectory "b/bonly"

    createFileWithContents "a/common/x" "x\na\nx"
    createFileWithContents "b/common/x" "x\nb\nx"

    createFileWithContents "a/aonly/afile" "a\na\na"
    createFileWithContents "b/bonly/bfile" "b\nb\nb"

    diff <- F.diffDirectories "a" "b"
    F.applyToDirectory diff "a"

    directoriesEqual <- areDirectoriesEqual "a" "b"
    (@?) directoriesEqual "Directories not equal after application"

tests :: TestTree
tests = testGroup "unit tests"
    [ -- diffing
      --    files
      testCase
        "Testing diffing individual files"
        (runTest testFileDiff)
    , testCase
        "Testing diffing the same file (identity diff)"
        (runTest testIdentityFileDiff)
    , testCase
        "Testing diffing individual files (nonexistent file case 1)"
        (runTest testNonexistentFileDiff1)
    , testCase
        "Testing diffing individual files (nonexistent file case 2)"
        (runTest testNonexistentFileDiff2)
    , testCase
        "Testing diffing individual files (nonexistent file case 3)"
        (runTest testNonexistentFileDiff3)
    , testCase
        "Testing diffing individual files (empty file case 1)"
        (runTest testFileDiffEmptyFiles)
    , testCase
        "Testing diffing individual files (empty file case 2)"
        (runTest testFileDiffEmptyBase)
    , testCase
        "Testing diffing individual files (empty file case 3)"
        (runTest testFileDiffEmptyComp)

    --     directories
    , testCase
        "Testing diffing the same directory (identity diff)"
        (runTest testIdentityDirDiff)
    , testCase
        "Testing diffing empty directories"
        (runTest testDirDiffEmptyDirectories)
    , testCase
        "Testing diffing for directories without files in them"
        (runTest testDirDiffNoFiles)
    , testCase
        "Testing diffing for directories"
        (runTest testDirDiff)

    -- patching
    , testCase
        "Testing patching individual files"
        (runTest testFileApply)
    , testCase
        "Testing patching algorithm for directories"
        (runTest testDirApply)

    -- alg
    --     composition
    , testCase
        "Testing sequence diffing composition"
        (testSequenceDiffComposition)
    , testCase
        "Testing sequence diffing composition (an edge case)"
        (testSequenceDiffCompositionEdgeCase1)
    , testCase
        "Testing file diffing composition"
        (runTest testFileDiffComposition)
    , testCase
        "Testing directory diffing composition"
        (runTest testDirectoryDiffComposition)
    ]

main :: IO ()
main = defaultMain tests
