{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Unboxed.MULinkedList where

import           Control.Monad (forM, forM_)
import           Control.Monad.ST (ST(..), runST)
import           Data.Foldable as F (toList)
import           Data.List (sortOn)
import           Data.Maybe (Maybe(..), isJust)
import           Data.STRef 
  (STRef(..), modifySTRef', newSTRef, readSTRef, writeSTRef)

import           MMZKDS.List (MList(..))
-- import           MMZKDS.MLinkedList (MUNode(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Unboxed.MURef
  (MU(..), MURef(..), modifyMURef, newMURef, readMURef, writeMURef)
import           MMZKDS.Unsafe (unsafeSTEq)
import           MMZKDS.Utilities (outOfBoundError)

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

--------------------------------------------------------------------------------
-- List Instance
--------------------------------------------------------------------------------

instance MU a s => MList MULinkedList a ST s where
  mClear :: MULinkedList a s -> ST s ()
  mClear (MULinkedList lR hR iR cR) = do
    writeMURef lR 0
    writeMURef iR $ -1
    hd <- newHead
    writeSTRef hR hd
    writeSTRef cR hd

  mDelete :: Int -> MULinkedList a s -> ST s (Maybe a)
  mDelete index mll@(MULinkedList lR _ iR cR) = do
    l <- readMURef lR
    if index < 0 || index >= l
      then return Nothing
      else do
        i   <- readMURef iR
        accessNode index mll
        cur <- readSTRef cR
        prv <- prevN cur
        nxt <- nextN cur
        writeSTRef (prevNRef nxt) prv
        writeSTRef (nextNRef prv) nxt
        writeMURef lR $ l - 1
        if index == i
          then writeMURef iR (-1) >> getHead mll >>= writeSTRef cR
          else return ()
        Just <$> nodeElem cur

  mGet :: MULinkedList a s -> Int -> ST s a
  mGet mll@(MULinkedList _ _ _ cR) index = do
    accessNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else nodeElem cur

  mIndicesOf :: Eq a => MULinkedList a s -> a -> ST s [Int]
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

  mInsert :: Int -> a -> MULinkedList a s -> ST s ()
  mInsert index e mll@(MULinkedList lR _ iR cR) = do
    l <- readMURef lR
    if index < 0 || index > l
      then outOfBoundError index
      else do
        accessNode index mll
        cur <- readSTRef cR
        prv <- prevN cur
        nR  <- newSTRef cur
        pR  <- newSTRef prv
        eR  <- newMURef e
        let newNode = MUNode pR eR nR
        writeSTRef (prevNRef cur) newNode
        writeSTRef (nextNRef prv) newNode
        writeSTRef cR newNode
        writeMURef lR $ l + 1

  mSet :: MULinkedList a s -> Int -> a -> ST s ()
  mSet mll@(MULinkedList _ _ _ cR) index e = do
    accessNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else do
        let MUNode _ eR _ = cur
        writeMURef eR e

  mSize :: MULinkedList a s -> ST s Int
  mSize (MULinkedList lR _ _ _)
    = readMURef lR

  mSortOn :: Ord b => (a -> b) -> MULinkedList a s -> ST s ()
  mSortOn f mll@(MULinkedList _ hR iR cR) = do
    i    <- readMURef iR
    list <- mToList mll
    let list' = sortOn (f . snd) $ zip [0..] list
    let ui'   = lookup i $ zip (fst <$> list') [0..]
    mll' <- mNewList $ snd <$> list'
    hd   <- getHead mll'
    writeSTRef hR $! hd
    writeSTRef cR $! hd
    writeMURef iR $ -1
    case ui' of
      Nothing -> return ()
      Just i' -> accessNode i' mll

  mSubList :: Int -> Int -> MULinkedList a s -> ST s (MULinkedList a s)
  mSubList inf sup mll@(MULinkedList _ _ _ cR) = do
    ls <- mSize mll
    forM [(max inf 0)..(min sup ls - 1)] 
      ((>> (readSTRef cR >>= nodeElem)) . flip accessNode mll) >>= mNewList
    
  mToList :: MULinkedList a s -> ST s [a]
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

  mNewList :: Foldable f => f a -> ST s (MULinkedList a s)
  mNewList xs = do
    mll <- emptyMULinkedList
    forM_ xs (flip mAppend mll)
    return mll

  -- Overwritten default method
  mAppend :: a -> MULinkedList a s -> ST s ()
  mAppend e (MULinkedList lR hR _ _) = do
    cur <- readSTRef hR
    prv <- prevN cur
    nR  <- newSTRef cur
    pR  <- newSTRef prv
    eR  <- newMURef e
    let newNode = MUNode pR eR nR
    writeSTRef (prevNRef cur) newNode
    writeSTRef (nextNRef prv) newNode
    modifyMURef lR succ

  -- Overwritten default method
  mIndexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
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
  mLastIndexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
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
  mPush :: a -> MULinkedList a s -> ST s ()
  mPush e (MULinkedList lR hR iR _) = do
    cur <- readSTRef hR
    nxt <- nextN cur
    pR  <- newSTRef cur
    nR  <- newSTRef nxt
    eR  <- newMURef e
    let newNode = MUNode pR eR nR
    writeSTRef (nextNRef cur) newNode
    writeSTRef (prevNRef nxt) newNode
    modifyMURef lR succ
    modifyMURef iR succ
    

--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance MU a s => MDS (MULinkedList a) s where
  copy :: MULinkedList a s -> ST s (MULinkedList a s)
  copy = (>>= new) . mToList

instance MU a s => MDSCons [a] (MULinkedList a) s where
  finish :: MULinkedList a s -> ST s [a]
  finish = mToList

  new :: [a] -> ST s (MULinkedList a s)
  new = mNewList


--------------------------------------------------------------------------------
-- Node-Specific Functions
--------------------------------------------------------------------------------

-- | Utility Function.
-- Set the reference to the current node at the given index.
-- If the index is out of bound, set it to head.
accessNode :: Int -> MULinkedList a s -> ST s ()
accessNode index (MULinkedList lR hR iR cR) = do
  l   <- readMURef lR
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
  i   <- readMURef iR
  nd' <- access' i l
  writeMURef iR $ if inBound then index else l
  writeSTRef cR nd'

-- | Utility Function.
-- Creates an empty linked list.
emptyMULinkedList :: ST s (MULinkedList a s)
emptyMULinkedList = do
  lR <- newMURef 0
  iR <- newMURef $ -1
  hd <- newHead
  hR <- newSTRef hd
  cR <- newSTRef hd
  return $ MULinkedList lR hR iR cR

-- | Utility Function.
-- Returns the head of a linked list
getHead :: MULinkedList a s -> ST s (MUNode a s)
getHead (MULinkedList _ hR _ _)
  = readSTRef hR

-- | Utility Function.
-- Tests if a node is head.
isHead :: MUNode a s -> Bool
isHead (MHead _ _)
  = True
isHead _
  = False

-- | Utility Function
-- Creates a head of a linked list.
newHead :: ST s (MUNode a s)
newHead = do
  pR <- newSTRef undefined
  nR <- newSTRef undefined
  let hNode = MHead pR nR
  writeSTRef pR hNode
  writeSTRef nR hNode
  return hNode

-- | Utility Function.
-- Returns the next node.
nextN :: MUNode a s -> ST s (MUNode a s)
nextN = readSTRef . nextNRef

-- | Utility Function.
-- The reference to the next node.
nextNRef :: MUNode a s -> STRef s (MUNode a s)
nextNRef (MHead _ nR)
  = nR
nextNRef (MUNode _ _ nR)
  = nR

-- | Utility Function.
-- Returns the previous node.
prevN :: MUNode a s -> ST s (MUNode a s)
prevN = readSTRef . prevNRef

-- | Utility Function.
-- The reference to the previous node.
prevNRef :: MUNode a s -> STRef s (MUNode a s)
prevNRef (MHead pR _)
  = pR
prevNRef (MUNode pR _ _)
  = pR

-- | Unsafe: Does not check if the node is head.
-- Get the element from an @MUNode@.
-- Pre: The node is not head.
nodeElem :: MU a s => MUNode a s -> ST s a
nodeElem (MUNode _ eR _)
  = readMURef eR


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

bar = runST $ do
  e  <- mNewList [1..10] :: ST s (MULinkedList Int s)
  accessNode 1 e
  f  <- mPopFront e
  let MULinkedList _ _ _ cR = e
  nd <- readSTRef cR
  b  <- return $ isHead nd
  el <- mToList e
  return (f, b, el)
