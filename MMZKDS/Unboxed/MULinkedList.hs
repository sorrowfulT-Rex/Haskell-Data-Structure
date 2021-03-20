{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Unboxed.MULinkedList where

import           Control.Monad (forM, forM_, liftM2, when, (<=<))
import           Control.Monad.ST (ST)
import           Data.Foldable as F (toList)
import           Data.List (elemIndex, sortOn)
import           Data.Maybe (fromJust, isJust)
import           Data.STRef
  (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

import           MMZKDS.List as L (MList(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Queue (MQueue(..))
import           MMZKDS.Unboxed.MURef
  (MU, MURef, modifyMURef, newMURef, readMURef, writeMURef)
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
  mDelete :: Int -> MULinkedList a s -> ST s (Maybe a)
  mDelete index mll@(MULinkedList lR _ iR cR) = do
    l <- readMURef lR
    if index < 0 || index >= l
      then return Nothing
      else do
        let preDelete i l
              | index == i = do
                cur <- readSTRef cR
                prv <- prevUN cur
                nxt <- nextUN cur
                writeMURef iR (-1)
                getHead mll >>= writeSTRef cR
                return (prv, cur, nxt)
              | i < index  = do
                accessUNode (index - 1) mll
                prv <- readSTRef cR
                cur <- nextUN prv
                nxt <- nextUN cur
                return (prv, cur, nxt)
              | otherwise  = do
                accessUNode (index + 1) mll
                nxt <- readSTRef cR
                cur <- prevUN nxt
                prv <- prevUN cur
                return (prv, cur, nxt)
        let del prv cur nxt = do
            writeSTRef (prevUNRef nxt) prv
            writeSTRef (nextUNRef prv) nxt
            Just <$> uNodeElem cur
        i <- readMURef iR
        (pre, cur, nxt) <- preDelete i l
        writeMURef lR $ l - 1
        del pre cur nxt

  mGet :: MULinkedList a s -> Int -> ST s a
  mGet mll@(MULinkedList _ _ _ cR) index = do
    accessUNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else uNodeElem cur

  mIndicesOf :: Eq a => MULinkedList a s -> a -> ST s [Int]
  mIndicesOf mll e = do
    let mIndicesOf' i node = do
        if isHead node
          then return []
          else if uNodeElem node `unsafeSTEq` return e
            then do
              nxt <- nextUN node
              rst <- mIndicesOf' (i + 1) nxt
              return $ i : rst
            else nextUN node >>= mIndicesOf' (i + 1)
    hd <- getHead mll
    nextUN hd >>= mIndicesOf' 0

  mInsert :: Int -> a -> MULinkedList a s -> ST s ()
  mInsert index e mll@(MULinkedList lR _ iR cR) = do
    l <- readMURef lR
    if index < 0 || index > l
      then outOfBoundError index
      else do
        accessUNode index mll
        cur <- readSTRef cR
        prv <- prevUN cur
        nR  <- newSTRef cur
        pR  <- newSTRef prv
        eR  <- newMURef e
        let newNode = MUNode pR eR nR
        writeSTRef (prevUNRef cur) newNode
        writeSTRef (nextUNRef prv) newNode
        writeSTRef cR newNode
        writeMURef lR $ l + 1

  mSet :: MULinkedList a s -> Int -> a -> ST s ()
  mSet mll@(MULinkedList _ _ _ cR) index e = do
    accessUNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else do
        let MUNode _ eR _ = cur
        writeMURef eR e

  mSortOn :: Ord b => (a -> b) -> MULinkedList a s -> ST s ()
  mSortOn f mll@(MULinkedList _ hR iR cR) = do
    i    <- readMURef iR
    list <- mToList mll
    let list' = sortOn (f . snd) $ zip [0..] list
    let ui'   = elemIndex i (fst <$> list')
    mll' <- mNewList $ snd <$> list'
    hd   <- getHead mll'
    writeSTRef hR $! hd
    writeSTRef cR $! hd
    writeMURef iR $ -1
    case ui' of
      Nothing -> return ()
      Just i' -> accessUNode i' mll

  mSubList :: Int -> Int -> MULinkedList a s -> ST s (MULinkedList a s)
  mSubList inf sup mll@(MULinkedList _ _ _ cR) = do
    ls <- size mll
    forM [(max inf 0)..(min sup ls - 1)]
      ((>> (readSTRef cR >>= uNodeElem)) . flip accessUNode mll) >>= mNewList

  -- Overwritten default method
  mAppend :: a -> MULinkedList a s -> ST s ()
  mAppend e (MULinkedList lR hR _ _) = do
    cur <- readSTRef hR
    prv <- prevUN cur
    nR  <- newSTRef cur
    pR  <- newSTRef prv
    eR  <- newMURef e
    let newNode = MUNode pR eR nR
    writeSTRef (prevUNRef cur) newNode
    writeSTRef (nextUNRef prv) newNode
    modifyMURef lR succ

  -- Overwritten default method
  mIndexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
  mIndexOf mll e = do
    let mIndexOf' i node
          | isHead node                          = return Nothing
          | uNodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise
            = nextUN node >>= mIndexOf' (i + 1)
    hd <- getHead mll
    nextUN hd >>= mIndexOf' 0

  -- Overwritten default method
  mLastIndexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
  mLastIndexOf mll e = do
    l <- size mll
    let mLastIndexOf' i node
          | isHead node                          = return Nothing
          | uNodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise 
            = prevUN node >>= mLastIndexOf' (i - 1)
    hd <- getHead mll
    prevUN hd >>= mLastIndexOf' (l - 1)

  -- Overwritten default method
  mPush :: a -> MULinkedList a s -> ST s ()
  mPush e (MULinkedList lR hR iR _) = do
    cur <- readSTRef hR
    nxt <- nextUN cur
    pR  <- newSTRef cur
    nR  <- newSTRef nxt
    eR  <- newMURef e
    let newNode = MUNode pR eR nR
    writeSTRef (nextUNRef cur) newNode
    writeSTRef (prevUNRef nxt) newNode
    modifyMURef lR succ
    i   <- readMURef iR
    when (i >= 0) $ modifyMURef iR succ


--------------------------------------------------------------------------------
-- MQueue Instance
--------------------------------------------------------------------------------

instance (Monad (m s), MList l a m s, MDS (l a) m s, MDSCons [a] (l a) m s)
  => MQueue l a m s where
  mDequeue = mPop

  mEnqueue = mPush

  mPeek m = do
    e <- mPop m
    when (isJust e) $ mAppend (fromJust e) m
    return e


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance MU a s => MDS (MULinkedList a) ST s where
  clear :: MULinkedList a s -> ST s ()
  clear (MULinkedList lR hR iR cR) = do
    writeMURef lR 0
    writeMURef iR $ -1
    hd <- newHead
    writeSTRef hR hd
    writeSTRef cR hd

  copy :: MULinkedList a s -> ST s (MULinkedList a s)
  copy = new <=< mToList

  size :: MULinkedList a s -> ST s Int
  size (MULinkedList lR _ _ _)
    = readMURef lR

instance MU a s => MDSCons [a] (MULinkedList a) ST s where
  finish :: MULinkedList a s -> ST s [a]
  finish mll = do
    let mToList' node = if isHead node
        then return []
        else liftM2 (:) (uNodeElem node) (nextUN node >>= mToList')
    getHead mll >>= nextUN >>= mToList'

  new :: [a] -> ST s (MULinkedList a s)
  new xs = do
    mll <- emptyMULinkedList
    forM_ xs (`mAppend` mll)
    return mll


--------------------------------------------------------------------------------
-- Node-Specific Functions
--------------------------------------------------------------------------------

-- | Utility Function.
-- Set the reference to the current node at the given index.
-- If the index is out of bound, set it to head.
accessUNode :: Int -> MULinkedList a s -> ST s ()
accessUNode index (MULinkedList lR hR iR cR) = do
  l   <- readMURef lR
  let inBound = index >= 0 && index < l
  let front' i nR
        | i == 0    = readSTRef nR
        | otherwise = readSTRef nR >>= front' (i - 1) . nextUNRef
  let back' i nR
        | i == 0    = readSTRef nR
        | otherwise = readSTRef nR >>= back' (i - 1) . prevUNRef
  let access' i l
        | not inBound              = readSTRef hR
        | index <= i `div` 2       = front' (1 + index) hR
        | index <= i               = back' (i - index) cR
        | index <= (i + l) `div` 2 = front' (index - i) cR
        | otherwise                = back' (l - index) hR
  i   <- readMURef iR
  nd' <- access' i l
  writeMURef iR $ if inBound then index else -1
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
nextUN :: MUNode a s -> ST s (MUNode a s)
nextUN = readSTRef . nextUNRef

-- | Utility Function.
-- The reference to the next node.
nextUNRef :: MUNode a s -> STRef s (MUNode a s)
nextUNRef (MHead _ nR)
  = nR
nextUNRef (MUNode _ _ nR)
  = nR

-- | Utility Function.
-- Returns the previous node.
prevUN :: MUNode a s -> ST s (MUNode a s)
prevUN = readSTRef . prevUNRef

-- | Utility Function.
-- The reference to the previous node.
prevUNRef :: MUNode a s -> STRef s (MUNode a s)
prevUNRef (MHead pR _)
  = pR
prevUNRef (MUNode pR _ _)
  = pR

-- | Unsafe: Does not check if the node is head.
-- Get the element from an @MUNode@.
-- Pre: The node is not head.
uNodeElem :: MU a s => MUNode a s -> ST s a
uNodeElem (MUNode _ eR _)
  = readMURef eR
