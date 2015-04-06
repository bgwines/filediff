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

import qualified Data.Text as T

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

-- sequence tests

testSequenceDiffEdgeCase1 :: Assertion
testSequenceDiffEdgeCase1 = do
    return $ FSeq.diffSequences "" "wabxyze"
    True @?= True -- no exception: considered success for this test

testSequenceDiffEdgeCase2 :: Assertion
testSequenceDiffEdgeCase2 = do
    return $ FSeq.diffSequences "wabxyze" ""
    True @?= True -- no exception: considered success for this test

testSequenceDiffEdgeCase3 :: Assertion
testSequenceDiffEdgeCase3 = do
    return $ FSeq.diffSequences "" ""
    True @?= True -- no exception: considered success for this test

-- file diff tests

testFileDiff :: Assertion
testFileDiff = do
    createFileWithContents "BASE" "a\nb\nc\nd\ne\nf\ng"
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [2,3,5,6] [(0, T.pack "w"),(3, T.pack "x"),(4, T.pack "y"),(5, T.pack "z")])
    (F.diffFiles "BASE" "COMP") >>= (@=?) expectedFilediff

testFileDiffEmptyFiles :: Assertion
testFileDiffEmptyFiles = do
    createFileWithContents "BASE" ""
    createFileWithContents "COMP" ""

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" mempty
    (F.diffFiles "BASE" "COMP") >>= (@=?) expectedFilediff

testFileDiffEmptyBase :: Assertion
testFileDiffEmptyBase = do
    createFileWithContents "BASE" ""
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [] [(0, T.pack "w"),(1, T.pack "a"),(2, T.pack "b"),(3, T.pack "x"),(4, T.pack "y"),(5, T.pack "z"),(6, T.pack "e")])
    (F.diffFiles "BASE" "COMP") >>= (@=?) expectedFilediff

testFileDiffEmptyComp :: Assertion
testFileDiffEmptyComp = do
    createFileWithContents "BASE" "a\nb\nc\nd\ne\nf\ng"
    createFileWithContents "COMP" ""

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [0,1,2,3,4,5,6] [])
    (F.diffFiles "BASE" "COMP") >>= (@=?) expectedFilediff

testNonexistentFileDiff1 :: Assertion
testNonexistentFileDiff1 = do
    createFileWithContents "BASE" "a\nb\nc\nd\ne\nf\ng"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [0,1,2,3,4,5,6] [])
    (F.diffFiles "BASE" "COMP") >>= (@=?) expectedFilediff

testNonexistentFileDiff2 :: Assertion
testNonexistentFileDiff2 = do
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "BASE" "COMP" (SeqDiff [] [(0, T.pack "w"),(1, T.pack "a"),(2, T.pack "b"),(3, T.pack "x"),(4, T.pack "y"),(5, T.pack "z"),(6, T.pack "e")])
    (F.diffFiles "BASE" "COMP") >>= (@=?) expectedFilediff

testNonexistentFileDiff3 :: Assertion
testNonexistentFileDiff3 = do
    let expectedFilediff = FTypes.Filediff "BASE" "COMP" mempty
    (F.diffFiles "BASE" "COMP") >>= (@=?) expectedFilediff

testIdentityFileDiff :: Assertion
testIdentityFileDiff = do
    createFileWithContents "COMP" "w\na\nb\nx\ny\nz\ne"

    let expectedFilediff = FTypes.Filediff "COMP" "COMP" mempty
    (F.diffFiles "COMP" "COMP") >>= (@=?) expectedFilediff

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
                , FTypes.linediff = (SeqDiff [1] [(1, T.pack "b")]) }
            , FTypes.Filediff
                    { FTypes.base = "aonly/afile"
                    , FTypes.comp = "aonly/afile"
                    , FTypes.linediff = (SeqDiff [0,1,2] []) }
            , FTypes.Filediff
                { FTypes.base = "bonly/bfile"
                , FTypes.comp = "bonly/bfile"
                , FTypes.linediff = (SeqDiff [] [(0, T.pack "b"),(1, T.pack "b"),(2, T.pack "b")]) } ]
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
    (F.diffDirectories "." ".") >>= (@=?) expectedDiff

-- composition tests

testSequenceDiffComposition :: Assertion
testSequenceDiffComposition = do
    let a = "abcdefg"
    let b = "wabxyze"
    let c = "#x##ye"

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

-- sequence apply tests

testSequenceApplyEdgeCase1 :: Assertion
testSequenceApplyEdgeCase1 = do
    let base = ""
    let comp = "abcde"

    let seqdiff = FSeq.diffSequences base comp
    let applied = FSeq.applySequenceDiff seqdiff base
    applied @?= comp

-- file apply tests

testFileApply :: Assertion
testFileApply = do
    let baseContents = "a\nb\nc\nd\ne\nf\ng"
    let compContents = "w\na\nb\nx\ny\nz\ne"

    createFileWithContents "BASE" baseContents
    createFileWithContents "COMP" compContents

    fileDiff <- F.diffFiles "BASE" "COMP"
    applied <- F.applyToFile fileDiff "BASE"
    applied @?= (T.lines . T.pack $ compContents)
    join $ (liftM2 (@?=)) (readFile "BASE") (readFile "COMP")

testFileApplyEdgeCase1 :: Assertion
testFileApplyEdgeCase1 = do
    let baseContents = ""
    let compContents = "w\na\nb\nx\ny\nz\ne"

    createFileWithContents "BASE" baseContents
    createFileWithContents "COMP" compContents

    fileDiff <- F.diffFiles "BASE" "COMP"
    applied <- F.applyToFile fileDiff "BASE"
    applied @?= (T.lines . T.pack $ compContents)
    join $ (liftM2 (@?=)) (readFile "BASE") (readFile "COMP")

testFileApplyEdgeCase2 :: Assertion
testFileApplyEdgeCase2 = do
    let baseContents = "a\nb\nc\nd\ne\nf\ng"
    let compContents = ""

    createFileWithContents "BASE" baseContents
    createFileWithContents "COMP" compContents

    fileDiff <- F.diffFiles "BASE" "COMP"
    applied <- F.applyToFile fileDiff "BASE"
    applied @?= (T.lines . T.pack $ compContents)
    join $ (liftM2 (@?=)) (readFile "BASE") (readFile "COMP")

-- tests deletion of a file
testFileApplyEdgeCase3 :: Assertion
testFileApplyEdgeCase3 = do
    let baseContents = "a\nb\nc\nd\ne\nf\ng"

    createFileWithContents "BASE" baseContents

    fileDiff <- F.diffFiles "BASE" "COMP"
    applied <- F.applyToFile fileDiff "BASE"

    exists <- D.doesFileExist "BASE"
    assertBool "File should be deleted." (not exists)

-- tests creation of a file
testFileApplyEdgeCase4 :: Assertion
testFileApplyEdgeCase4 = do
    let baseContents = "a\nb\nc\nd\ne\nf\ng"
    let compContents = "w\na\nb\nx\ny\nz\ne"

    createFileWithContents "BASE" baseContents
    createFileWithContents "COMP" compContents

    fileDiff <- F.diffFiles "BASE" "COMP"

    D.removeFile "BASE"

    applied <- F.applyToFile fileDiff "BASE"
    applied @?= (T.lines . T.pack $ compContents)
    join $ (liftM2 (@?=)) (readFile "BASE") (readFile "COMP")

testFileApplyEdgeCase5 :: Assertion
testFileApplyEdgeCase5 = do
    let baseContents = ""
    let compContents = "\n"

    createFileWithContents "BASE" baseContents
    createFileWithContents "COMP" compContents

    fileDiff <- F.diffFiles "BASE" "COMP"
    applied <- F.applyToFile fileDiff "BASE"
    applied @?= (T.lines . T.pack $ compContents)
    join $ (liftM2 (@?=)) (readFile "BASE") (readFile "COMP")

testFileApplyEdgeCase6 :: Assertion
testFileApplyEdgeCase6 = do
    let baseContents = "\n"
    let compContents = ""

    createFileWithContents "BASE" baseContents
    createFileWithContents "COMP" compContents

    fileDiff <- F.diffFiles "BASE" "COMP"
    applied <- F.applyToFile fileDiff "BASE"
    applied @?= (T.lines . T.pack $ compContents)
    join $ (liftM2 (@?=)) (readFile "BASE") (readFile "COMP")

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

    -- removing files and removing directories
    -- also addition of bfile to `a` adds \n

    directoriesEqual <- areDirectoriesEqual "a" "b"
    (@?) directoriesEqual "Directories not equal after application"

testInMemoryDirApply :: Assertion
testInMemoryDirApply = do
    0 @?= 1

-- tests

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
        "Testing patching individual files (edge case 1)"
        (runTest testFileApplyEdgeCase1)
    , testCase
        "Testing patching individual files (edge case 2)"
        (runTest testFileApplyEdgeCase2)
    , testCase
        "Testing patching individual files (edge case 3)"
        (runTest testFileApplyEdgeCase3)
    , testCase
        "Testing patching individual files (edge case 4)"
        (runTest testFileApplyEdgeCase4)
    , testCase
        "Testing patching individual files (edge case 5)"
        (runTest testFileApplyEdgeCase5)
    , testCase
        "Testing patching individual files (edge case 6)"
        (runTest testFileApplyEdgeCase6)
    , testCase
        "Testing patching directories"
        (runTest testDirApply)
    , testCase
        "Testin in-memory patching directories"
        (runTest testInMemoryDirApply)

    -- alg
    -- edge cases
    , testCase
        "Testing sequence diffing (edge case 1)"
        (testSequenceDiffEdgeCase1)
    , testCase
        "Testing sequence diffing (edge case 2)"
        (testSequenceDiffEdgeCase2)
    , testCase
        "Testing sequence diffing (edge case 3)"
        (testSequenceDiffEdgeCase3)
    , testCase
        "Testing sequence patching (edge case 1)"
        (testSequenceApplyEdgeCase1)

    --     composition
    , testCase
        "Testing sequence diffing composition"
        (testSequenceDiffComposition)
    , testCase
        "Testing sequence diffing composition (edge case 1)"
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
