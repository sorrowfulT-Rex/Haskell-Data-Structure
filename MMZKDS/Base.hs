module MMZKDS.Base 
  (ArrayList(..), AVLSet(..), MArrayList(..), MHeapPQ(..), MLinkedList(..),
   MNode(..)
  ) where

import           Data.Array (Array)
import           Data.Array.ST (STArray)
import           Data.STRef (STRef)

import           MMZKDS.Unboxed.MURef (MURef)
import           MMZKDS.Utilities (GBTN)

-- | @ArrayList@ is a data structure implementing the 'List' class with an
-- internal array.
-- All operations that requires mutation on the @ArrayList@ (exept @clear@ and
-- @deepClear@) requires generating a new @ArrayList@, which is very costly 
-- (always O(n)). Therefore it is recommended to use the mutable version
-- 'MArrayList' for frequent state updates.
--
data ArrayList e = ArrayList {-# UNPACK #-} !Int (Array Int e)

-- | An immutable set structure implemented with an internal AVL-tree.
-- It is expected that the type of its elements is an instance of 'Ord'.
-- It has O(log n) adding, O(log n) deleting, O(log n) searching, O(n * log n) 
-- union and intersection, and O(n * log n) construction from list.
-- 
newtype AVLSet e = AVLSet (GBTN e)

-- | @MArrayList@ is a data structure implementing the 'MList' class with an
-- internal @STArray@.
-- It has O(1) random access, O(1) appending/popping, O(n) inserting/deleting,
-- O(n) searching, and O(n * log n) sorting.
--
data MArrayList e s = MArrayList (MURef s Int) (STRef s (STArray s Int e))

-- | 'MHeapPQ' is a min-heap implementing the 'MPriorityQueue' class.
-- The heap is implemented with an internal @STArray@.
-- It is expected that the type of its elements is an instance of 'Ord'.
-- It may adds an element to anywhere in the array, but it always pops the 
-- "smallest" element.
-- It has O(log n) adding, O(log n) popping, and O(n) construction from list.
-- 
data MHeapPQ e s = MHeapPQ {
  mHeapS :: MURef s Int,
  mHeapA :: STRef s (STArray s Int e)
  }

-- | @MLinkedList@ is a doubly-linked circular list implementing the 'MList'
-- class.
-- It has O(1) access to front and rear, O(1) insertion/deletion to front and
-- rear, O(n) random access, O(n) insertion/deletion in general, O(n) search,
-- and O(n * log n) sorting.
-- It remembers the node most recently accessed, and operating at the vicinity 
-- of this node is O(1).
--
data MLinkedList e s
  = MLinkedList
    (MURef s Int)  -- ^ Length of the Linked-List
    (STRef s (MNode e s)) -- ^ Point to the head node
    (MURef s Int)  -- ^ Index of the most recently accessed node
    (STRef s (MNode e s)) -- ^ Pointer to the most recently accessed node

-- | @MNode@ represents a single node in @MLinkedList@.
--
data MNode e s
  = MHead (STRef s (MNode e s)) (STRef s (MNode e s))
  | MNode (STRef s (MNode e s)) (STRef s e) (STRef s (MNode e s))
