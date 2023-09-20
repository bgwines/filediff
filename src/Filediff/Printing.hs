{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for printing 'Filediff' data types to the console.
module Filediff.Printing
( printDiff
, printFilediff
, printListdiff
) where

import Rainbow

import Zora.List as ZL (merge_by)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Internal as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Function ((&))

import Filediff.Types

-- | Prints a 'ListDiff' 'Line'. Prints with colors and some formatting.
printListdiff :: ListDiff Line -> IO ()
printListdiff (ListDiff dels adds) = do
    let deletedLines = zip (repeat "del") dels
    let addedLines   = zip (repeat "add") adds

    let interleaved = ZL.merge_by mergeCmpFn deletedLines addedLines
    mapM_ printLine interleaved
    where
        mergeCmpFn :: (String, (Int, Line)) -> (String, (Int, Line)) -> Ordering
        mergeCmpFn (_, (i, _)) (_, (j, _)) = i `compare` j

        printLine :: (String, (Int, Line)) -> IO ()
        printLine (t, (i, line)) = do
            putChunkLn $ chunk line' & color
            where
                line' :: Line
                line' = if t == "del"
                    then (T.pack "- ") <> line
                    else (T.pack "+ ") <> line

                color :: Chunk -> Chunk
                color = if t == "del"
                    then fore red
                    else fore green

-- | Prints a 'Filediff'. Prints with colors and some formatting.
printFilediff :: Filediff -> IO ()
printFilediff (Filediff base comp change) = do
    let diffType :: T.Text = case change of {
        Add _ -> "(file addition)";
        Mod _ -> "(file modification)";
        Del _ -> "(file deletion)"; }
    let diffDescription :: T.Text = "diff a/" <> (T.pack base) <> " b/" <> (T.pack comp) <> " " <> diffType
    putChunkLn $ chunk diffDescription & bold

    let delLine :: T.Text = "--- a/" <> (T.pack base)
    let addLine :: T.Text = "+++ b/" <> (T.pack comp)
    putChunkLn $ chunk delLine & bold
    putChunkLn $ chunk addLine & bold

    printListdiff $ listDiff change
    putChunkLn $ chunk ("" :: T.Text)

-- | Prints a 'Diff'. Prints with colors and some formatting.
printDiff :: Diff -> IO ()
printDiff = mapM_ printFilediff . filediffs
