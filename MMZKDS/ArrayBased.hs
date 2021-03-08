{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.ArrayBased where 

import           Control.Monad (forM_)
import           Control.Monad.ST (ST(..))
import           Data.Array.ST (MArray(..), STArray(..), readArray, writeArray)
import           Data.Foldable (toList)

import           MMZKDS.List (maximumOn)


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Utility Function. 
-- Returns an error indicating the length of the array has exceeds the limit.
-- For arrays indexed on @Int@, however, this is not going to happen in practice
-- since it requires more than 1024 PiB memory.
-- 
arrayLengthOverflowError :: a
arrayLengthOverflowError = error "Length of array has overflowed!"

-- | Unsafe: Does not check conduct bound check for array.
-- Takes an @Int@ as the starting index, an element, an @Int@ as the last index
-- to be written to, and an @Int@-indexed @STArray@, pushes all elements since
-- the starting index (inclusive) to the last index (exclusive) to the next 
-- slot, opening a vacancy at the starting index, where it puts the given
--  element to this index.
-- Pre: The index bounds are valid.
--
unsafeAddST :: (Monad m, MArray (STArray s) a m) 
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

-- | Unsafe: Does not check conduct bound check for array.
-- Takes an @Int@-indexed @STArray@ as the starting array, another
-- @Int@-indexed @STArray@ as the destination, and a tuple of @Int@s as the
-- lower and upper bound (both inclusive) of the range of indices.
-- Copies the elements in the starting array within the range to the
-- destination whose index starts from 0.
-- Pre: The index bounds are valid and the destination array is large enough
-- to hold the number of elements.
--
unsafeCopyArray :: (Monad m, MArray (STArray s) a m)
                => STArray s Int a 
                -> STArray s Int a 
                -> (Int, Int) 
                -> m ()
unsafeCopyArray arrST resST (inf, sup) = do
  forM_ (zip [0..] [inf..sup]) $ 
    \(i, i') -> readArray arrST i >>= writeArray resST i'

-- | Unsafe: Does not check if the array satisfies the pre-condition.
-- TODO...
-- Takes a ordering function and an @Int@-indexed @STArray@, sorts the array.
-- Pre: The array must be @Int@-indexed from 0.
--
{-# INLINE unsafeHeapSort #-}
unsafeHeapSort :: (Monad m, Ord b, MArray (STArray s) a m) 
               => (a -> b) 
               -> Int 
               -> STArray s Int a 
               -> m ()
unsafeHeapSort f sup arrST
  = toMaxHeap 0 >> heapSort sup
  where
    toMaxHeap i
      | i > sup = return ()
      | otherwise 
        = toMaxHeap lc >> toMaxHeap rc >> fixMaxHeap i sup
        where
          lc = 2 * i + 1
          rc = lc + 1
    fixMaxHeap i l
      | lc > l     = return ()
      | lc < l     = do
        cv <- readArray arrST i
        lv <- readArray arrST lc
        rv <- readArray arrST rc
        let (mv, mi) = maximumOn (f . fst) [(cv, i), (lv, lc), (rv, rc)]
        if mi == i
          then return ()
          else 
            writeArray arrST i mv >> writeArray arrST mi cv >> fixMaxHeap mi l
      | otherwise = do
        cv <- readArray arrST i
        lv <- readArray arrST lc
        let (mv, mi) = maximumOn (f . fst) [(cv, i), (lv, lc)]
        if mi == i
          then return ()
          else 
            writeArray arrST i mv >> writeArray arrST mi cv >> fixMaxHeap mi l
      where
        lc = 2 * i + 1
        rc = lc + 1
    heapSort 0 = return ()
    heapSort i = do
      v0 <- readArray arrST 0
      vi <- readArray arrST i
      writeArray arrST 0 vi
      writeArray arrST i v0
      fixMaxHeap 0 (i - 1)
      heapSort (i - 1)

-- | Unsafe: Does not check conduct bound check for array.
-- Takes an @Int@ as the starting index, an element, an @Int@ as the last index
-- to be written to, and an @Int@-indexed @STArray@, pushes all elements since
-- the starting index (exclusive) to the last index (inclusive) to the previous 
-- slot, effectively removing the original element at the starting index.
-- Pre: The index bounds are valid and the last index is less than the physical
-- length of the array minus 1.
--
unsafeRemoveST :: (Monad m, MArray (STArray s) a m) 
               => Int 
               -> Int 
               -> STArray s Int a -> m ()
unsafeRemoveST index lastIndexOf arrST
  = forM_ [index..lastIndexOf] $ \i -> do
    v <- readArray arrST (i + 1)
    writeArray arrST i v


--------------------------------------------------------------------------------
-- ArrayBased Type Class
--------------------------------------------------------------------------------

-- | 'ArrayBased' is a type class for immutable array-based data structure.
-- It provides methods to allocate new arrays for length adjustment.
-- Instances of 'ArrayBased' is required to implement 'Foldable'.
-- Minimal implementation requires @deepClear@, @newWithSize@ and 
-- @physicalSize@.
--
class Foldable a => ArrayBased a where
  -- | Returns a new structure that is truly empty; in other words, all elements
  -- are physically removed from the structure.
  --
  deepClear :: a e -> a e

  -- | Takes an @Int@ as length and an instance of 'Foldable', creates a new 
  -- structure containing the elements in the 'Foldable' and the representing
  -- array has at least the length specified by the argument.
  --
  newWithSize  :: Foldable f => Int -> f e -> a e

  -- | Returns the physical size of the structure, in other words, the length
  -- of the representing array.
  --
  physicalSize :: a e -> Int

  -- | Optional method.
  -- | Takes an Int as length and a structure, returns a structure containing
  -- the same elements but with at least the length specified by the argument.
  --
  resize :: Int -> a e -> a e
  resize = (. toList) . newWithSize

-- | 'MArrayBased' is a type class for mutable @STArray@-based data structure.
-- It provides methods to allocate new arrays for length adjustment, and to copy
-- the structure that retains it's physical size.
-- Instances of 'ArrayBased' is required to implement 'Foldable'.
-- Minimal implementation requires @mDeepClear@, @newMWithSize@, 
-- @mPhysicalSize@, @mResize@ and @trueCopy@.
--
class MArrayBased a where
  -- | Truly empties the structure; in other words, all elements are physically 
  -- removed from the structure.
  --
  mDeepClear :: a e s -> ST s ()

  -- | Takes an @Int@ as length and an instance of 'Foldable', creates a new 
  -- structure containing the elements in the 'Foldable' and the representing
  -- array has at least the length specified by the argument.
  -- 
  newMWithSize :: Foldable f => Int -> f e -> ST s (a e s)

  -- | Returns the physical size of the structure, in other words, the length
  -- of the representing array.
  -- 
  mPhysicalSize :: a e s -> ST s Int

  -- | Takes an @Int@ as length and a structure, modifies the structure such that
  -- it has at least the length specified by the argument.
  --
  mResize :: Int -> a e s -> ST s (a e s)

  -- | Create a new mutable data structure from the given mutable data 
  -- structure, retaining the physical size.
  --
  trueCopy :: a e s -> ST s (a e s)
