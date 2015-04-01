{-# LANGUAGE InstanceSigs #-}

-- | Data types used by `Filediff`
module Filediff.Types
( Filediff(..)
, Diff(..)
, Line
, Error
) where

import qualified Data.Text as T

import Data.List (intersect, sortBy)

import Data.Monoid
import Control.Applicative

import Filediff.Sequence (SeqDiff(..))

import Data.MemoCombinators (Memo, wrap)
import Data.MemoCombinators.Class (MemoTable, table, memoize)

-- | The basic data type for a difference between two files. The
-- | `FilePath` is the "base" file in the base-comp comparison, and
-- | is the file to which the patch will be applied. Deletions: a list
-- | of indices at which to remove elements. Additions: each line to add
-- | comes with the index at which it will eventually reside.
data Filediff = Filediff {
    base :: FilePath,
    comp :: FilePath,
    linediff :: SeqDiff Line
} deriving (Eq, Show)

-- TODO: is this mathematically correct?
instance Monoid Filediff where
    mempty :: Filediff
    mempty = Filediff "" "" mempty

    mappend :: Filediff -> Filediff -> Filediff
    mappend fd1 fd2 =
        if comp fd1 /= base fd2
            then error $ "`comp` of filediff 1 is not `base` of filediff 2: " ++ (show fd1) ++ " vs. " ++ (show fd2) 
            else Filediff {
                base = base fd1,
                comp = comp fd2,
                linediff = linediff fd1 `mappend` linediff fd2 }

-- | A data type for differences between directories
data Diff = Diff {
    -- relative to directories being diffed
    filediffs :: [Filediff]
} deriving (Show)

instance Eq Diff where
    (==) :: Diff -> Diff -> Bool
    (==) a b
        = sortBy cmp (filediffs a) == sortBy cmp (filediffs b)
        where
            cmp :: Filediff -> Filediff -> Ordering
            cmp a b = if base a /= base b
                then base a `compare` base b
                else comp a `compare` comp b

instance MemoTable T.Text where
    --       :: (ByteString -> r) -> ByteString -> r
    -- table :: Memo ByteString
    table = wrap T.pack T.unpack table

instance Monoid Diff where
    mempty :: Diff
    mempty = Diff []

    mappend :: Diff -> Diff -> Diff
    mappend (Diff aFilediffs) (Diff bFilediffs)
        = Diff $ filediffs
        where
            filediffs :: [Filediff]
            filediffs
                = filter ((/=) mempty . linediff)
                $ exclusion ++ (map (uncurry mappend) intersection)

            exclusion :: [Filediff]
            exclusion
                =  (excludeBy dirsEqual aFilediffs bFilediffs)
                ++ (excludeBy dirsEqual bFilediffs aFilediffs)

            intersection :: [(Filediff, Filediff)]
            intersection = intersectBy dirsEqual aFilediffs bFilediffs

            dirsEqual :: Filediff -> Filediff -> Bool
            dirsEqual
                (Filediff aBase aComp _)
                (Filediff bBase bComp _)
                = (aBase == bBase) && (aComp == bComp)

excludeBy  :: (a -> a -> Bool) -> [a] -> [a] -> [a]
excludeBy _ [] _ = []
excludeBy f (x:xs) ys =
    if any (f x) ys
        then excludeBy f xs ys
        else x : excludeBy f xs ys

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [(a, a)]
intersectBy f a b
    = filter (uncurry f)
    $ (\x y -> (x,y)) <$> a <*> b

-- | Data type for a line
type Line = T.Text

-- | Basic error type
type Error = String
