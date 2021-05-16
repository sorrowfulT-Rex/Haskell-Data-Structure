{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables#-}

module MMZKDS.Unboxed.Base 
  ( MUArrayList(..), MUHeapPQ(..), MULinkedList(..), UArrayList(..), MUNode(..)
  , arrayListFreeze, arrayListThaw, unsafeArrayListFreeze, unsafeArrayListThaw
  ) where

import           Control.Monad.ST (ST)
import           Data.Array.ST
  (STUArray, MArray, freeze, thaw)    
import           Data.Array.Unboxed (IArray, UArray)
import           Data.Array.Unsafe (unsafeFreeze, unsafeThaw)
import           Data.STRef (STRef, newSTRef, readSTRef)
import           Data.Array.Base (STUArray, UArray)

import           MMZKDS.Unboxed.STURef (STU, STURef, newSTURef, readSTURef)

-- | @MUArrayList@ is a data structure implementing the 'MList' class with an
-- internal @STUArray@.
-- It has O(1) random access, amortised O(1) appending/popping, O(n) 
-- inserting/deleting, O(n) searching, and O(n * log n) sorting.
--
data MUArrayList e s = MUArrayList (STURef s Int) (STRef s (STUArray s Int e))

-- | 'MUHeapPQ' is a min-heap implementing the 'MPriorityQueue' class with
-- unboxed elements.
-- The heap is implemented with an internal @STUArray@.
-- It may adds an element to anywhere in the array, but it always pops the 
-- "smallest" element.
-- It has O(log n) adding, O(log n) popping, and O(n) construction from list.
-- 
data MUHeapPQ e s = MUHeapPQ 
  { mHeapS :: STURef s Int
  , mHeapA :: STRef s (STUArray s Int e)
  }

-- | @MULinkedList@ is a doubly-linked circular list implementing the 'MList'
--  class, containing unboxed elements.
-- It has O(1) access to front and rear, O(1) insertion/deletion to front and
-- rear, O(n) random access, O(n) insertion/deletion in general, O(n) search,
-- and O(n * log n) sorting.
-- It remembers the node most recently accessed, and operating at the vicinity 
-- of this node is O(1).
--
data MULinkedList e s
  = MULinkedList
    (STURef s Int)  -- ^ Length of the Linked-List
    (STRef s (MUNode e s)) -- ^ Pointer to the head node
    (STURef s Int)  -- ^ Index of the most recently accessed node
    (STRef s (MUNode e s)) -- ^ Pointer to the most recently accessed node

-- | @MUNode@ represents a single node in @MULinkedList@.
--
data MUNode e s
  = MHead (STRef s (MUNode e s)) (STRef s (MUNode e s))
  | MUNode (STRef s (MUNode e s)) (STURef s e) (STRef s (MUNode e s))

-- | @UArrayList@ is a data structure implementing the 'List' class with an
-- internal array storing unboxed values.
-- All operations that requires mutation on the @UArrayList@ (exept @clear@ and
-- @deepClear@) requires generating a new @UArrayList@, which is very costly 
-- (always O(n)). Therefore it is recommended to use the mutable version
-- 'MUArrayList' for frequent state updates.
--
data UArrayList e = UArrayList {-# UNPACK #-} !Int (UArray Int e)



--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

-- | Makes an immutable @UArrayList@ from a mutable @MUArrayList@ by copying. 
--
arrayListFreeze :: forall a s. (IArray UArray a, MArray (STUArray s) a (ST s))
                => MUArrayList a s
                -> ST s (UArrayList a)
arrayListFreeze (MUArrayList lR arrR) = do
  l     <- readSTURef lR
  arrST <- readSTRef arrR
  arr   <- freeze arrST
  return $ UArrayList l arr

-- | Makes a mutable @MUArrayList@ from an immutable @ArrayList@ by copying. 
--
arrayListThaw :: forall a s. (IArray UArray a, MArray (STUArray s) a (ST s))
              => UArrayList a
              -> ST s (MUArrayList a s)
arrayListThaw (UArrayList l arr) = do
  arrST <- thaw arr :: ST s (STUArray s Int a)
  lR    <- newSTURef l
  arrR  <- newSTRef arrST
  return $ MUArrayList lR arrR

-- | Unsafe Function.
-- Makes an immutable @UArrayList@ from a mutable @MUArrayList@, perhaps without
-- copying.
-- The original mutable list should not be used ever since.
--
unsafeArrayListFreeze :: forall a s. (IArray UArray a, STU a s)
                      => MUArrayList a s
                      -> ST s (UArrayList a)
unsafeArrayListFreeze (MUArrayList lR arrR) = do
  l     <- readSTURef lR
  arrST <- readSTRef arrR
  arr   <- unsafeFreeze arrST
  return $ UArrayList l arr

-- | Unsafe Function.
-- Makes a mutable @MUArrayList@ from an immutable @UArrayList@, perhaps without
-- copying.
-- The original immutable list should not be used ever since.
--
unsafeArrayListThaw :: forall a s. (IArray UArray a, STU a s)
                    => UArrayList a
                    -> ST s (MUArrayList a s)
unsafeArrayListThaw (UArrayList l arr) = do
  arrST <- unsafeThaw arr
  lR    <- newSTURef l
  arrR  <- newSTRef arrST
  return $ MUArrayList lR arrR
