{-# LANGUAGE FlexibleContexts #-}

module MMZKDS.Unsafe where

import           Control.Monad (forM_)
import           Control.Monad.ST (ST(..))
import           Control.Monad.ST.Unsafe (unsafeIOToST)
import           Data.Array.ST (MArray(..), STArray(..), readArray, writeArray)

import           System.Random (Random(..), StdGen(..), newStdGen, randomR)

-- | Unsafe: Does not conduct bound check for array.
-- Takes an @Int@ as the starting index, an element, an @Int@ as the last index
-- to be written to, and an @Int@-indexed @STArray@, pushes all elements since
-- the starting index (inclusive) to the last index (exclusive) to the next 
-- slot, opening a vacancy at the starting index, where it puts the given
--  element to this index.
-- Pre: The index bounds are valid.
--
unsafeAddST :: (MArray (STArray s) a m) 
            => Int 
            -> a 
            -> Int 
            -> STArray s Int a 
            -> m ()
unsafeAddST index e lastIndexOf arrST = do
  forM_ [lastIndexOf, (lastIndexOf - 1)..index] $ \i -> do
    v <- readArray arrST i
    writeArray arrST (i + 1) v
  writeArray arrST index e

-- | Unsafe: Does not conduct bound check for array.
-- Takes an @Int@-indexed @STArray@ as the starting array, another
-- @Int@-indexed @STArray@ as the destination, and a tuple of @Int@s as the
-- lower and upper bound (both inclusive) of the range of indices.
-- Copies the elements in the starting array within the range to the
-- destination whose index starts from 0.
-- Pre: The index bounds are valid and the destination array is large enough
-- to hold the number of elements.
--
unsafeCopyArray :: (MArray (STArray s) a m)
                => STArray s Int a 
                -> STArray s Int a 
                -> (Int, Int) 
                -> m ()
unsafeCopyArray arrST resST (inf, sup) = do
  forM_ (zip [0..] [inf..sup]) $ 
    \(i, i') -> readArray arrST i >>= writeArray resST i'

-- | Unsafe Function.
-- Random generator.
unsafeGenST :: ST s StdGen
unsafeGenST = unsafeIOToST newStdGen

-- | Unsafe Function.
-- Generate random value between a range.
randRangeST :: (Random a) => (a, a) -> StdGen -> ST s (a, StdGen)
randRangeST = (return .) . randomR

-- -- | Unsafe: Does not check if the array satisfies the pre-condition.
-- -- Takes a ordering function, an index upper bound, and an @Int@-indexed mutable
-- -- array, sorts the array with heap-sort.
-- -- Pre: The array must be @Int@-indexed from 0.
-- --
-- {-# INLINE unsafeHeapSort #-}
-- unsafeHeapSort :: (Ord b, MArray r a m) 
--                => (a -> b) 
--                -> Int 
--                -> r Int a 
--                -> m ()
-- unsafeHeapSort f sup arrST
--   = toMaxHeap 0 >> forM_ [sup, sup - 1..1] heapSort
--   where
--     toMaxHeap i
--       | lc > sup = return ()
--       | otherwise 
--         = toMaxHeap lc >> toMaxHeap (lc + 1) >> fixMaxHeap i sup
--         where
--           lc = 2 * i + 1
--     fixMaxHeap i l = do
--       let lc = 2 * i + 1
--       let rc = 2 * i + 2
--       cv <- readArray arrST i
--       let swapper
--             | lc > l   = return (False, undefined, undefined)
--             | rc > l   = do
--               lv <- readArray arrST lc
--               if f cv >= f lv
--                 then return (False, undefined, undefined)
--                 else return (True, lc, lv)
--             | otherwise = do
--               lv <- readArray arrST lc
--               rv <- readArray arrST rc
--               if f cv >= f lv && f cv >= f rv
--                 then return (False, undefined, undefined)
--                 else if f lv >= f rv 
--                     then return (True, lc, lv)
--                     else return (True, rc, rv)
--       s <- swapper
--       case s of
--         (False, _, _) -> return ()
--         (True, lo, v) -> do
--           writeArray arrST i v
--           writeArray arrST lo cv
--           fixMaxHeap lo l
--     heapSort i = do
--       v0 <- readArray arrST 0
--       vi <- readArray arrST i
--       writeArray arrST 0 vi
--       writeArray arrST i v0
--       fixMaxHeap 0 (i - 1)

-- | Unsafe: Does not check if the array satisfies the pre-condition.
-- Takes a ordering function, an index lower bound, an index upper bound, and 
-- an @Int@-indexed @STArray@, sorts the array with quick-sort.
-- Pre: The array must be @Int@-indexed from 0.
--
{-# INLINE unsafeQuickSort #-}
unsafeQuickSort :: (Ord b) 
               => (a -> b) 
               -> Int
               -> Int 
               -> STArray s Int a 
               -> ST s ()
unsafeQuickSort f inf sup arrST
  = unsafeGenST >>= worker inf sup 
  where
    worker inf sup gen
      | inf + 1 >= sup = return ()
      | otherwise      = do
        let getPivot = do
            (r, gen) <- randRangeST (inf, sup - 1) gen
            vi       <- readArray arrST inf
            vp       <- readArray arrST $ (inf + sup) `div` 2
            writeArray arrST inf vp
            writeArray arrST ((inf + sup) `div` 2) vi
            return (vp, gen)
        let shuffleAround i j pv
              | i + 1 == j = return i
              | otherwise  = do
                vi <- readArray arrST (i + 1)
                if f vi <= pv
                  then shuffleAround (i + 1) j pv
                  else do
                    vj <- readArray arrST (j - 1)
                    writeArray arrST (i + 1) vj
                    writeArray arrST (j - 1) vi
                    shuffleAround i (j - 1) pv
        (p, gen) <- getPivot
        pi       <- shuffleAround inf sup $! f p
        vp       <- readArray arrST pi
        writeArray arrST inf vp
        writeArray arrST pi p
        worker inf pi gen
        worker (pi + 1) sup gen

-- | Unsafe: Does not conduct bound check for array.
-- Takes an @Int@ as the starting index, an element, an @Int@ as the last index
-- to be written to, and an @Int@-indexed @STArray@, pushes all elements since
-- the starting index (exclusive) to the last index (inclusive) to the previous 
-- slot, effectively removing the original element at the starting index.
-- Pre: The index bounds are valid and the last index is less than the physical
-- length of the array minus 1.
--
unsafeRemoveST :: (MArray (STArray s) a m) 
               => Int 
               -> Int 
               -> STArray s Int a -> m ()
unsafeRemoveST index lastIndexOf arrST
  = forM_ [index..lastIndexOf] $ \i -> do
    v <- readArray arrST (i + 1)
    writeArray arrST i v
