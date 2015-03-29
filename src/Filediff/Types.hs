{-# LANGUAGE InstanceSigs #-}

-- | Data types used by `Filediff`
module Filediff.Types
( Filediff(..)
, Diff(..)
, (|.|)
, Line
, Error
) where

import Data.Monoid

import Filediff.Sequence (SeqDiff(..))

-- | The basic data type for a difference between two files. The
-- | `FilePath` is the "base" file in the base-comp comparison, and
-- | is the file to which the patch will be applied. Deletions: a list
-- | of indices at which to remove elements. Additions: each line to add
-- | comes with the index at which it will eventually reside.
data Filediff = Filediff {
    base :: FilePath, -- TODO: relative to root of repo
    comp :: FilePath,
    linediff :: SeqDiff Line
} deriving (Eq, Show)

-- TODO: is this conceptually correct?
instance Monoid Filediff where
    mempty :: Filediff
    mempty = Filediff "" "" mempty

    mappend :: Filediff -> Filediff -> Filediff
    mappend fd1 fd2 =
        if comp fd1 /= base fd2
            then error "`comp` of filediff 1 is not `base` of filediff 2"
            else Filediff {
                base = base fd1,
                comp = comp fd2,
                linediff = linediff fd1 `mappend` linediff fd2 }

-- | A generalized diff data type: can store the difference between
-- | directories, or just between files (singleton `filediffs` field).
data Diff = Diff {
    baseDir :: FilePath, -- ./.horse/HEAD-contents
    compDir :: FilePath, -- ./
    filediffs :: [Filediff],
    dirdiffs :: [Diff] -- right?
} deriving (Eq, Show)

-- TODO
(|.|) :: Diff -> Diff -> Diff
(|.|) a b = a

-- | Data type for a line
type Line = String

-- | Basic error type
type Error = String
