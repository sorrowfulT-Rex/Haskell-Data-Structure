{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MLinkedList where

import           Control.Monad (forM, forM_)
import           Control.Monad.ST (ST(..), runST)
import           Data.List (sortOn)
import           Data.Maybe (Maybe(..), isJust)
import           Data.STRef 
  (STRef(..), modifySTRef', newSTRef, readSTRef, writeSTRef)

import           MMZKDS.List as L (MList(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Queue (MQueue(..))
import           MMZKDS.Unsafe (unsafeSTEq)
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
  | MNode (STRef s (MNode e s)) (STRef s e) (STRef s (MNode e s)) 


--------------------------------------------------------------------------------
-- MList Instance
--------------------------------------------------------------------------------

instance MList MLinkedList a ST s where
  mClear :: MLinkedList a s -> ST s ()
  mClear (MLinkedList lR hR iR cR) = do
    writeSTRef lR 0
    writeSTRef iR $ -1
    hd <- newHead
    writeSTRef hR hd
    writeSTRef cR hd

  mDelete :: Int -> MLinkedList a s -> ST s (Maybe a)
  mDelete index mll@(MLinkedList lR _ iR cR) = do
    l <- readSTRef lR
    if index < 0 || index >= l
      then return Nothing
      else do
        i   <- readSTRef iR
        accessNode index mll
        cur <- readSTRef cR
        prv <- prevN cur
        nxt <- nextN cur
        writeSTRef (prevNRef nxt) prv
        writeSTRef (nextNRef prv) nxt
        writeSTRef lR $! l - 1
        if index == i
          then writeSTRef iR (-1) >> getHead mll >>= writeSTRef cR
          else return ()
        Just <$> nodeElem cur

  mGet :: MLinkedList a s -> Int -> ST s a
  mGet mll@(MLinkedList _ _ _ cR) index = do
    accessNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else nodeElem cur

  mIndicesOf :: Eq a => MLinkedList a s -> a -> ST s [Int]
  mIndicesOf mll e = do
    let mIndicesOf' i node = do
        if isHead node
          then return []
          else if nodeElem node `unsafeSTEq` return e
            then do
              nxt <- nextN node
              rst <- mIndicesOf' (i + 1) nxt
              return $ i : rst
            else nextN node >>= mIndicesOf' (i + 1)
    hd <- getHead mll
    nextN hd >>= mIndicesOf' 0

  mInsert :: Int -> a -> MLinkedList a s -> ST s ()
  mInsert index e mll@(MLinkedList lR _ iR cR) = do
    l <- readSTRef lR
    if index < 0 || index > l
      then outOfBoundError index
      else do
        accessNode index mll
        cur <- readSTRef cR
        prv <- prevN cur
        nR  <- newSTRef cur
        pR  <- newSTRef prv
        eR  <- newSTRef e
        let newNode = MNode pR eR nR
        writeSTRef (prevNRef cur) newNode
        writeSTRef (nextNRef prv) newNode
        writeSTRef cR newNode
        writeSTRef lR $! l + 1

  mSet :: MLinkedList a s -> Int -> a -> ST s ()
  mSet mll@(MLinkedList _ _ _ cR) index e = do
    accessNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else do
        let MNode _ eR _ = cur
        writeSTRef eR e

  mSize :: MLinkedList a s -> ST s Int
  mSize (MLinkedList lR _ _ _)
    = readSTRef lR

  mSortOn :: Ord b => (a -> b) -> MLinkedList a s -> ST s ()
  mSortOn f mll@(MLinkedList _ hR iR cR) = do
    i    <- readSTRef iR
    list <- mToList mll
    let list' = sortOn (f . snd) $ zip [0..] list
    let ui'   = lookup i $ zip (fst <$> list') [0..]
    mll' <- mNewList $ snd <$> list'
    hd   <- getHead mll'
    writeSTRef hR $! hd
    writeSTRef cR $! hd
    writeSTRef iR $ -1
    case ui' of
      Nothing -> return ()
      Just i' -> accessNode i' mll

  mSubList :: Int -> Int -> MLinkedList a s -> ST s (MLinkedList a s)
  mSubList inf sup mll@(MLinkedList _ _ _ cR) = do
    ls <- mSize mll
    forM [(max inf 0)..(min sup ls - 1)] 
      ((>> (readSTRef cR >>= nodeElem)) . flip accessNode mll) >>= mNewList
    
  mToList :: MLinkedList a s -> ST s [a]
  mToList mll = do
    let mToList' node = do
        if isHead node
          then return []
          else do
            nxt <- nextN node
            e   <- nodeElem node
            rst <- mToList' nxt
            return $ e : rst
    hd <- getHead mll
    nextN hd >>= mToList'

  mNewList :: Foldable f => f a -> ST s (MLinkedList a s)
  mNewList xs = do
    mll <- emptyMLinkedList
    forM_ xs (flip mAppend mll)
    return mll

  -- Overwritten default method
  mAppend :: a -> MLinkedList a s -> ST s ()
  mAppend e (MLinkedList lR hR _ _) = do
    cur <- readSTRef hR
    prv <- prevN cur
    nR  <- newSTRef cur
    pR  <- newSTRef prv
    eR  <- newSTRef e
    let newNode = MNode pR eR nR
    writeSTRef (prevNRef cur) newNode
    writeSTRef (nextNRef prv) newNode
    modifySTRef' lR succ

  -- Overwritten default method
  mIndexOf :: Eq a => MLinkedList a s -> a -> ST s (Maybe Int)
  mIndexOf mll e = do
    let mIndexOf' i node = do
        if isHead node
          then return Nothing
          else if nodeElem node `unsafeSTEq` return e
            then return $ Just i
            else nextN node >>= mIndexOf' (i + 1)
    hd <- getHead mll
    nextN hd >>= mIndexOf' 0

  -- Overwritten default method
  mLastIndexOf :: Eq a => MLinkedList a s -> a -> ST s (Maybe Int)
  mLastIndexOf mll e = do
    l <- mSize mll
    let mLastIndexOf' i node = do
        if isHead node
          then return Nothing
          else if nodeElem node `unsafeSTEq` return e
            then return $ Just i
            else prevN node >>= mLastIndexOf' (i - 1)
    hd <- getHead mll
    prevN hd >>= mLastIndexOf' (l - 1)

  -- Overwritten default method
  mPush :: a -> MLinkedList a s -> ST s ()
  mPush e (MLinkedList lR hR iR _) = do
    cur <- readSTRef hR
    nxt <- nextN cur
    pR  <- newSTRef cur
    nR  <- newSTRef nxt
    eR  <- newSTRef e
    let newNode = MNode pR eR nR
    writeSTRef (nextNRef cur) newNode
    writeSTRef (prevNRef nxt) newNode
    modifySTRef' lR succ
    modifySTRef' iR succ


--------------------------------------------------------------------------------
-- MQueue Instance
--------------------------------------------------------------------------------

instance MQueue MLinkedList a ST s where
  mAdd :: a -> MLinkedList a s -> ST s ()
  mAdd = mPush

  mClear :: MLinkedList a s -> ST s ()
  mClear = L.mClear

  mPop :: MLinkedList a s -> ST s (Maybe a)
  mPop = L.mPop


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance MDS (MLinkedList a) s where
  copy :: MLinkedList a s -> ST s (MLinkedList a s)
  copy = (>>= new) . mToList

instance MDSCons [a] (MLinkedList a) s where
  finish :: MLinkedList a s -> ST s [a]
  finish = mToList

  new :: [a] -> ST s (MLinkedList a s)
  new = mNewList


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
emptyMLinkedList :: ST s (MLinkedList a s)
emptyMLinkedList = do
  lR <- newSTRef 0
  iR <- newSTRef $ -1
  hd <- newHead
  hR <- newSTRef hd
  cR <- newSTRef hd
  return $ MLinkedList lR hR iR cR

-- | Utility Function.
-- Returns the head of a linked list
getHead :: MLinkedList a s -> ST s (MNode a s)
getHead (MLinkedList _ hR _ _)
  = readSTRef hR

-- | Utility Function.
-- Tests if a node is head.
isHead :: MNode a s -> Bool
isHead (MHead _ _)
  = True
isHead _
  = False

-- | Utility Function
-- Creates a head of a linked list.
newHead :: ST s (MNode a s)
newHead = do
  pR <- newSTRef undefined
  nR <- newSTRef undefined
  let hNode = MHead pR nR
  writeSTRef pR hNode
  writeSTRef nR hNode
  return hNode

-- | Utility Function.
-- Returns the next node.
nextN :: MNode a s -> ST s (MNode a s)
nextN = readSTRef . nextNRef

-- | Utility Function.
-- The reference to the next node.
nextNRef :: MNode a s -> STRef s (MNode a s)
nextNRef (MHead _ nR)
  = nR
nextNRef (MNode _ _ nR)
  = nR

-- | Utility Function.
-- Returns the previous node.
prevN :: MNode a s -> ST s (MNode a s)
prevN = readSTRef . prevNRef

-- | Utility Function.
-- The reference to the previous node.
prevNRef :: MNode a s -> STRef s (MNode a s)
prevNRef (MHead pR _)
  = pR
prevNRef (MNode pR _ _)
  = pR

-- | Unsafe: Does not check if the node is head.
-- Get the element from an @MNode@.
-- Pre: The node is not head.
nodeElem :: MNode a s -> ST s a
nodeElem (MNode _ eR _)
  = readSTRef eR


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

bar = runST $ do
  e  <- mNewList [1..10] :: ST s (MLinkedList Int s)
  accessNode 0 e
  f  <- mPopFront e
  let MLinkedList _ _ _ cR = e
  nd <- readSTRef cR
  b  <- return $ isHead nd
  el <- mToList e
  return (f, b, el)
