module MMZKDS.Unboxed.Base 
  (MUArrayList(..), MUHeapPQ(..), MULinkedList(..), UArrayList(..), MUNode(..)
  ) where

import           Data.STRef (STRef)
import           Data.Array.Base (STUArray, UArray)

import           MMZKDS.Unboxed.MURef (MURef)

-- | @MUArrayList@ is a data structure implementing the 'MList' class with an
-- internal @STUArray@.
-- It has O(1) random access, O(1) appending/popping, O(n) inserting/deleting,
-- O(n) searching, and O(n * log n) sorting.
--
data MUArrayList e s = MUArrayList (MURef s Int) (STRef s (STUArray s Int e))

-- | 'MUHeapPQ' is a min-heap implementing the 'MPriorityQueue' class with
-- unboxed elements.
-- The heap is implemented with an internal @STUArray@.
-- It may adds an element to anywhere in the array, but it always pops the 
-- "smallest" element.
-- It has O(log n) adding, O(log n) popping, and O(n) construction from list.
-- 
data MUHeapPQ e s = MUHeapPQ {
  mHeapS :: MURef s Int,
  mHeapA :: STRef s (STUArray s Int e)
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
    (MURef s Int)  -- ^ Length of the Linked-List
    (STRef s (MUNode e s)) -- ^ Point to the head node
    (MURef s Int)  -- ^ Index of the most recently accessed node
    (STRef s (MUNode e s)) -- ^ Pointer to the most recently accessed node

-- | @MUNode@ represents a single node in @MULinkedList@.
--
data MUNode e s
  = MHead (STRef s (MUNode e s)) (STRef s (MUNode e s))
  | MUNode (STRef s (MUNode e s)) (MURef s e) (STRef s (MUNode e s))

-- | @UArrayList@ is a data structure implementing the 'List' class with an
-- internal array storing unboxed values.
-- All operations that requires mutation on the @UArrayList@ (exept @clear@ and
-- @deepClear@) requires generating a new @UArrayList@, which is very costly 
-- (always O(n)). Therefore it is recommended to use the mutable version
-- 'MUArrayList' for frequent state updates.
--
data UArrayList e = UArrayList {-# UNPACK #-} !Int (UArray Int e)
