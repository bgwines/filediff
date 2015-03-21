{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- imports

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- qualified imports

import qualified System.IO as IO
import qualified System.Directory as Dir

-- imported functions

import System.Exit (exitSuccess)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Control.Monad ((>>=), return, when)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

import Data.Either.Combinators (isLeft, fromLeft)

import qualified Filediff as F
import qualified Filediff.Types as FTypes

-- | Runs a test in its own empty directory.
-- | Effectively, it isolates it from all other tests.
runTest :: Assertion -> Assertion
runTest t = do
    testDirectory <- getTestDirectory
    Dir.createDirectory testDirectory
    Dir.setCurrentDirectory testDirectory

    t

    Dir.setCurrentDirectory ".."
    Dir.removeDirectoryRecursive testDirectory
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

testDiff :: Assertion
testDiff = do
    let oldContents = "a\nb\nc\nd\ne\nf\ng"
    let newContents = "w\na\nb\nx\ny\nz\ne"

    let addedFile = "OLD"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle oldContents
    IO.hClose handle

    let addedFile = "NEW"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle newContents
    IO.hClose handle

    (F.diffFiles "OLD" "NEW") >>= (@?=) (FTypes.Diff "OLD" [2,3,5,6] [(0,"w"),(3,"x"),(4,"y"),(5,"z")])

testApply :: Assertion
testApply = do
    let oldContents = "a\nb\nc\nd\ne\nf\ng"
    let newContents = "w\na\nb\nx\ny\nz\ne"

    let addedFile = "OLD"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle oldContents
    IO.hClose handle

    let addedFile = "NEW"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle newContents
    IO.hClose handle

    diff <- F.diffFiles "OLD" "NEW"
    applied <- F.apply diff "OLD"
    applied @?= (lines newContents)

tests :: TestTree
tests = testGroup "unit tests"
    [ testCase
        "Testing diffing algorithm"
        (runTest testDiff) 
    , testCase
        "Testing patching algorithm"
        (runTest testApply) ]

main :: IO ()
main = defaultMain tests
