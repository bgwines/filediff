{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for printing 'Filediff' data types to the console.
module Filediff.Printing
( printDiff
, printFilediff
, printSeqdiff
) where

import Rainbow

import Zora.List as ZL (merge_by)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as ByteString

import Filediff.Types
import Filediff.Sequence

-- | Prints a 'SeqDiff Line'. Prints with colors and some formatting.
printSeqdiff :: SeqDiff Line -> IO ()
printSeqdiff (SeqDiff dels adds) = do
    let deletedLines = zip (repeat "del") (zip dels (repeat ""))
    let addedLines   = zip (repeat "add") (adds)

    let interleaved = ZL.merge_by mergeCmpFn deletedLines addedLines
    mapM_ printLine interleaved
    where
        mergeCmpFn :: (String, (Int, Line)) -> (String, (Int, Line)) -> Ordering
        mergeCmpFn (_, (i, _)) (_, (j, _)) = i `compare` j

        printLine :: (String, (Int, Line)) -> IO ()
        printLine (t, (i, line)) = do
            putChunkLn $ chunk (T.encodeUtf8 line') & color
            where
                line' :: Line
                line' = if t == "del"
                    then T.pack $ "- <Line " ++ (show i) ++ ">"
                    else (T.pack "+ ") <> line

                color :: Chunk a -> Chunk a
                color = if t == "del"
                    then fore red
                    else fore green

-- | Prints a 'Filediff'. Prints with colors and some formatting.
printFilediff :: Filediff -> IO ()
printFilediff (Filediff base comp change) = do
    let diffType :: ByteString = case change of {
        Add _ -> "(file addition)";
        Mod _ -> "(file modification)";
        Del _ -> "(file deletion)"; }
    let diffDescription :: ByteString = "diff a/" <> (ByteString.pack base) <> " b/" <> (ByteString.pack comp) <> " " <> diffType
    putChunkLn $ chunk diffDescription & fore brightWhite & bold

    let delLine :: ByteString = "--- a/" <> (ByteString.pack base)
    let addLine :: ByteString = "+++ b/" <> (ByteString.pack comp)
    putChunkLn $ chunk delLine & fore brightWhite & bold
    putChunkLn $ chunk addLine & fore brightWhite & bold

    printSeqdiff $ seqDiff change
    putChunkLn $ chunk ("" :: ByteString)

-- | Prints a 'Diff'. Prints with colors and some formatting.
printDiff :: Diff -> IO ()
printDiff = mapM_ printFilediff . filediffs
