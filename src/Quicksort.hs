{-# LANGUAGE BangPatterns #-}

-- | In-place quicksort.

module QuickSort (quickSort) where

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.List (sort)
import           Data.Vector (Vector)
import qualified Data.Vector as V (toList)
import qualified Data.Vector.Generic as V(freeze)
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as V
import           Prelude hiding (read)
import           Test.QuickCheck

--------------------------------------------------------------------------------
-- Implementation

-- | In-place quicksort the given vector in some mutation-permitting
-- monad.
quickSort :: (PrimMonad m,Ord a) => MVector (PrimState m) a -> m ()
quickSort vec =
  qsort vec 0 (V.length vec)
  where qsort array begin end =
          when (end > begin)
               (do let startP = begin + ((end - begin) `div` 2)
                   -- ^ I chose to simply choose the pivot as the
                   -- median, no cleverness or randomness here. AIUI
                   -- Sedgewick recommends this.
                   pivot <- partition array begin end startP
                   -- The condition below is recommended by Sedgewick:
                   --
                   -- To make sure at most O(log n) space is used,
                   -- recurse first into the smaller side of the
                   -- partition, then use a tail call to recurse into
                   -- the other.
                   if pivot - begin > end - pivot + 1
                      then do qsort array (pivot + 1) end
                              qsort array begin pivot
                      else do qsort array begin pivot
                              qsort array (pivot + 1) end)

-- | Swap elements in the array until all elements <pivot are to the
-- left of pivot.
partition :: (PrimMonad m,Ord a) => V.MVector (PrimState m) a -> Int -> Int -> Int -> m Int
partition array begin end pivot =
  do piv <- V.read array pivot
     V.swap array pivot (end - 1)
     store <- for begin (end - 1) begin
                  (\ix !store ->
                     do v <- V.read array ix
                        if v <= piv
                           then do V.swap array store ix
                                   return (store + 1)
                           else return store)
     V.swap array (end - 1) store
     return store
  where for from to state m = go from state
          where go i state =
                  if i < to
                     then do state' <- m i state
                             go (i + 1) state'
                     else return state

--------------------------------------------------------------------------------
-- Tests

-- | Test that sorting some list of ints is equivalent to @sort xs@.
quickSortProp :: [Int] -> Bool
quickSortProp xs =
  sort xs ==
  V.toList (runST (do arr <- thaw xs
                      quickSort arr
                      freeze arr))

--------------------------------------------------------------------------------
-- Example

-- | Works in either the IO or ST monad!
main :: IO ()
main =
  do quickCheck quickSortProp
     ioVector <- do arr <- thaw [1,7,2,4,1,8,5,2]
                    quickSort arr
                    freeze arr
     print ioVector
     print (runST (do arr <- thaw [1,7,2,4,1,8,5,2]
                      quickSort arr
                      freeze arr))

-- | Handy function to construct a mutable vector from a list.
thaw :: (PrimMonad m)
     => [Int] -> m (MVector (PrimState m) Int)
thaw is =
  do array <- V.new (length is)
     forM_ (zip [0 ..] is)
           (\(i,v) -> V.write array i (v :: Int))
     return array

-- | More specific type for freezing.
freeze :: (PrimMonad m)
       => MVector (PrimState m) a -> m (Vector a)
freeze = V.freeze
