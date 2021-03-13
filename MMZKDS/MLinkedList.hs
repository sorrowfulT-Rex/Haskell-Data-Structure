{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MLinkedList where

import           Control.Monad (forM_, join)
import           Control.Monad.ST (ST(..), runST)
import           Data.Foldable as F (toList)
import           Data.STRef 
  (STRef(..), modifySTRef', newSTRef, readSTRef, writeSTRef)

import           MMZKDS.List (MList(..))
import           MMZKDS.MDT (MDT(..), MDTCons(..))
import           MMZKDS.Utilities (outOfBoundError)

-- | @MLinkedList@ is a doubly-linked circular list implementing the 'MList'
--  class.
-- It has O(1) access to front and rear, O(1) insertion/deletion to front and
-- rear, O(n) random access, O(n) insertion/deletion in general, O(n) search,
-- and O(n * log n) sorting.
-- It remembers the node most recently accessed, and operating at the vicinity 
-- of this node is O(1).
--
data MLinkedList e s 
  = MLinkedList 
    (STRef s Int)  -- ^ Length of the Linked-List
    (STRef s (MNode e s)) -- ^ Point to the head node
    (STRef s Int)  -- ^ Index of the most recently accessed node
    (STRef s (MNode e s)) -- ^ Pointer to the most recently accessed node

-- | @MNode@ represents a single node in @MLinkedList@.
--
data MNode e s 
  = MHead (STRef s (MNode e s)) (STRef s (MNode e s)) 
  | MNode (STRef s (MNode e s)) e (STRef s (MNode e s)) 


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance MList MLinkedList a ST s where
  mAdd :: Int -> a -> MLinkedList a s -> ST s ()
  mAdd index e mll = do
    accessNode index mll
    let MLinkedList lR _ iR cR = mll
    cur <- readSTRef cR
    l   <- readSTRef lR
    if index < 0 || index > l
      then outOfBoundError index
      else do
        prv <- prevN cur
        nR  <- newSTRef cur
        pR  <- newSTRef prv
        let newNode = MNode pR e nR
        writeSTRef (prevNRef cur) newNode
        writeSTRef (nextNRef prv) newNode
        writeSTRef cR newNode
        writeSTRef lR (l + 1)

  mClear :: MLinkedList a s -> ST s ()
  mClear (MLinkedList lR hR iR cR) = do
    writeSTRef lR 0
    writeSTRef iR 0
    hd <- newHead
    writeSTRef hR hd
    writeSTRef cR hd
    
  mToList :: MLinkedList a s -> ST s [a]
  mToList (MLinkedList _ hR _ _) = do
    let mToList' node = do
        if isHead node
          then return []
          else do
            nxt <- nextN node
            rst <- mToList' nxt
            return $ (nodeElem node) : rst
    hd <- readSTRef hR
    join $ mToList' <$> nextN hd

  newMList :: Foldable f => f a -> ST s (MLinkedList a s)
  newMList xs = do
    mll <- emptyMLinkedList
    forM_ xs (flip mAppend mll)
    return mll

  -- Overwritten default methods
  mAppend :: a -> MLinkedList a s -> ST s ()
  mAppend e (MLinkedList lR hR _ _) = do
    cur <- readSTRef hR
    prv <- prevN cur
    nR  <- newSTRef cur
    pR  <- newSTRef prv
    let newNode = MNode pR e nR
    writeSTRef (prevNRef cur) newNode
    writeSTRef (nextNRef prv) newNode
    modifySTRef' lR succ

  -- Overwritten default methods
  mPush :: a -> MLinkedList a s -> ST s ()
  mPush e (MLinkedList lR hR iR _) = do
    cur <- readSTRef hR
    nxt <- nextN cur
    pR  <- newSTRef cur
    nR  <- newSTRef nxt
    let newNode = MNode pR e nR
    writeSTRef (nextNRef cur) newNode
    writeSTRef (prevNRef nxt) newNode
    modifySTRef' lR succ
    modifySTRef' iR succ
    

--------------------------------------------------------------------------------
-- MDT Functions
--------------------------------------------------------------------------------

instance MDT (MLinkedList a) s where
  copy :: MLinkedList a s -> ST s (MLinkedList a s)
  copy = (>>= newMList) . mToList

instance Foldable f => MDTCons (f a) (MLinkedList a) s where
  new :: f a -> ST s (MLinkedList a s)
  new = newMList


--------------------------------------------------------------------------------
-- Node-Specific Functions
--------------------------------------------------------------------------------

-- | Utility Function.
-- Set the reference to the current node at the given index.
-- If the index is out of bound, set it to head.
accessNode :: Int -> MLinkedList a s -> ST s ()
accessNode index (MLinkedList lR hR iR cR) = do
  l   <- readSTRef lR
  let inBound = index >= 0 && index < l
  let front' i nR
        | i == 0    = readSTRef nR
        | otherwise = readSTRef nR >>= return . nextNRef >>= front' (i - 1)
  let back' i nR
        | i == 0    = readSTRef nR
        | otherwise = readSTRef nR >>= return . prevNRef >>= back' (i - 1)
  let access' i l
        | not inBound              = readSTRef hR
        | index <= i `div` 2       = front' (1 + index) hR
        | index <= i               = back' (i - index) cR
        | index <= (i + l) `div` 2 = front' (index - i) cR
        | otherwise                = back' (l - index) hR
  i   <- readSTRef iR
  nd' <- access' i l
  writeSTRef iR $ if inBound then index else l
  writeSTRef cR nd'

-- | Utility Function.
-- Creates an empty linked list.
emptyMLinkedList :: ST s (MLinkedList e s)
emptyMLinkedList = do
  lR <- newSTRef 0
  iR <- newSTRef 0
  hd <- newHead
  hR <- newSTRef hd
  cR <- newSTRef hd
  return $ MLinkedList lR hR iR cR

-- | Utility Function.
-- Tests if a node is head.
isHead :: MNode e s -> Bool
isHead (MHead _ _)
  = True
isHead _
  = False

-- | Utility Function
-- Creates a head of a linked list.
newHead :: ST s (MNode e s)
newHead = do
  pR <- newSTRef undefined
  nR <- newSTRef undefined
  let hNode = MHead pR nR
  writeSTRef pR hNode
  writeSTRef nR hNode
  return hNode

-- | Utility Function.
-- Returns the next node.
nextN :: MNode e s -> ST s (MNode e s)
nextN = readSTRef . nextNRef

-- | Utility Function.
-- The reference to the next node.
nextNRef :: MNode e s -> STRef s (MNode e s)
nextNRef (MHead _ nR)
  = nR
nextNRef (MNode _ _ nR)
  = nR

-- | Utility Function.
-- Returns the previous node.
prevN :: MNode e s -> ST s (MNode e s)
prevN = readSTRef . prevNRef

-- | Utility Function.
-- The reference to the previous node.
prevNRef :: MNode e s -> STRef s (MNode e s)
prevNRef (MHead pR _)
  = pR
prevNRef (MNode pR _ _)
  = pR

-- | Unsafe: Does not check if the node is head.
-- Get the element from an @MNode@.
-- Pre: The node is not head.
nodeElem :: MNode e s -> e
nodeElem (MNode _ e _)
  = e


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

bar = runST $ do
  e <- newMList [100..110] :: ST s (MLinkedList Int s)
  mClear e
  mAdd 0 2 e
  mAdd 1 3 e
  mAdd 0 4 e
  mPush 10 e
  mAdd 5 100 e
  el <- mToList e
  return el
