{-# LANGUAGE ScopedTypeVariables #-}

module Filediff
( -- * basic operations
  diffFiles
, diffDirectories
, apply

-- * additional operations
, compose
) where

import Data.List ((\\))

import Control.Applicative ((<$>), (<*>))

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- Filediff imports

import Filediff.Types
import Filediff.Utils(longestCommonSubsequence, nonSubsequenceIndices)

-- * helper functions

-- | Compute the difference between the two files (more
-- | specifically, the minimal number of changes to make to transform the
-- | file residing at the location specified by the first
-- | parameter into the second). Returns a fail state if either or both of
-- | the parametres point to a directory, not a file.
diffFiles :: FilePath -> FilePath -> IO Diff
diffFiles f1 f2 = do
    f1lines <- lines <$> readFile f1
    f2lines <- lines <$> readFile f2
    let common = longestCommonSubsequence f1lines f2lines
    let toDel = nonSubsequenceIndices common f1lines
    let toAdd = getProgressiveIndicesToAdd common f2lines
    return $ Diff toDel toAdd
    where
        -- | λ add
        -- | [(0,"w"),(3,"x"),(4,"y")]
        -- | λ common
        -- | ["a","b","e"]
        getProgressiveIndicesToAdd :: (Eq a) => [a] -> [a] -> [(Int, a)]
        getProgressiveIndicesToAdd sub super =
            map (\i -> (i, super !! i)) $ nonSubsequenceIndices sub super


-- | Compute the difference between the two directories (more
-- | specifically, the minimal number of changes to make to transform the
-- | directory residing at the location specified by the first
-- | parameter into the second). Returns a fail state if either or both of
-- | the parametres point to a file, not a directory.
diffDirectories :: FilePath -> FilePath -> IO Diff
diffDirectories d1 d2 = error "not yet implemented"

-- | Apply a diff to a directory or file
apply :: Diff -> FilePath -> EitherT Error IO ()
apply d filepath = error "not yet implemented"
    where
        -- | Best explained by example:
        -- |
        -- |     > insertAtProgressiveIndices [(1,'a'),(3,'b')] "def"
        -- |     "daebf"
        insertAtProgressiveIndices :: forall a. (Eq a) => [(Int, a)] -> [a] -> [a]
        insertAtProgressiveIndices = insertAtProgressiveIndices' 0

        insertAtProgressiveIndices' :: Int -> [(Int, a)] -> [a] -> [a]
        insertAtProgressiveIndices' _ [] dest = dest
        insertAtProgressiveIndices' curr src@((i,s):src') dest@(d:dest') =
            if i == curr
                then s : insertAtProgressiveIndices' (succ curr) src' dest
                else d : insertAtProgressiveIndices' (succ curr) src dest'

-- * additional operations

-- | Composition may fail
compose :: Diff -> Diff -> Maybe Diff
compose a b = error "not yet implemented"
