{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Data types used by `Filediff`
module Filediff.Types
( Filediff(..)
, Diff(..)
, FileChange(..)
, seqDiff
, isDel
, isMod
, isAdd
, Line
, Error
) where

import GHC.Generics

import Data.Default

import qualified Data.Text as T

import Data.List (intersect, sortBy)

import Data.Monoid
import Control.Applicative

import Filediff.Sequence (SeqDiff(..))

import Data.MemoCombinators (Memo, wrap)
import Data.MemoCombinators.Class (MemoTable, table, memoize)

-- | The basic data type for a difference between two files. The
--   `FilePath` is the "base" file in the base-comp comparison, and
--   is the file to which the patch will be applied. Deletions: a list
--   of indices at which to remove elements. Additions: each line to add
--   comes with the index at which it will eventually reside.
data Filediff = Filediff {
    base :: FilePath,
    comp :: FilePath,
    change :: FileChange
} deriving (Eq, Show, Generic)

-- | The types and sets of changes possible between two files.
data FileChange
    = Del (SeqDiff Line)
    | Mod (SeqDiff Line)
    | Add (SeqDiff Line) deriving (Eq, Show, Generic)

-- | Gets the 'SeqDiff' stored in a 'FileChange'.
seqDiff :: FileChange -> SeqDiff Line
seqDiff (Del diff) = diff
seqDiff (Mod diff) = diff
seqDiff (Add diff) = diff

-- | Whether a 'FileChange' is a deletion or not.
isDel :: FileChange -> Bool
isDel (Del _) = True
isDel (Mod _) = False
isDel (Add _) = False
-- | Whether a 'FileChange' is a modification or not.
isMod :: FileChange -> Bool
isMod (Del _) = False
isMod (Mod _) = True
isMod (Add _) = False
-- | Whether a 'FileChange' is a addition or not.
isAdd :: FileChange -> Bool
isAdd (Del _) = False
isAdd (Mod _) = False
isAdd (Add _) = True

instance Monoid FileChange where
    mempty :: FileChange
    mempty = Mod mempty -- no changes (no add / del); identity diff

    mappend :: FileChange -> FileChange -> FileChange
    mappend (Del _    ) (Del _    ) = error "del ++ del"
    mappend (Del _    ) (Mod _    ) = error "del ++ mod"
    mappend (Del diff1) (Add diff2) = Mod $ diff1 `mappend` diff2
    mappend (Mod _    ) (Del diff2) = Del diff2
    mappend (Mod diff1) (Mod diff2) = Mod $ diff1 `mappend` diff2
    mappend (Mod _    ) (Add _    ) = error "mod ++ add"
    mappend (Add _    ) (Del _    ) = mempty -- will be filtered out during directory composition. Yes; this isn't ideal, but it's at least clean.
    mappend (Add diff1) (Mod diff2) = Add $ diff1 `mappend` diff2
    mappend (Add _    ) (Add _    ) = error "add ++ add"

-- TODO: is this mathematically correct?
instance Monoid Filediff where
    mempty :: Filediff
    mempty = Filediff "" "" (Mod mempty)

    mappend :: Filediff -> Filediff -> Filediff
    mappend fd1 fd2 =
        if comp fd1 /= base fd2
            then error $ "`comp` of filediff 1 is not `base` of filediff 2: " ++ (show fd1) ++ " vs. " ++ (show fd2) 
            else Filediff {
                base = base fd1,
                comp = comp fd2,
                change = change fd1 `mappend` change fd2 }

-- | A data type for differences between directories
data Diff = Diff {
    -- relative to directories being diffed
    filediffs :: [Filediff]
} deriving (Show, Generic)

instance Eq Diff where
    (==) :: Diff -> Diff -> Bool
    (==) a b
        = sortBy cmp (filediffs a) == sortBy cmp (filediffs b)
        where
            cmp :: Filediff -> Filediff -> Ordering
            cmp a b = if base a /= base b
                then base a `compare` base b
                else comp a `compare` comp b

instance Default Diff where
    def :: Diff
    def = Diff []

instance MemoTable T.Text where
    table :: Memo T.Text
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
                = filter ((/=) mempty . change)
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
