{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Unboxed.MULinkedList (MULinkedList) where

import           Control.Monad (forM, forM_, liftM2, when, (<=<))
import           Control.Monad.ST (ST)
import           Data.Foldable as F (toList)
import           Data.List (elemIndex, sortOn)
import           Data.Maybe (fromJust, isJust)
import           Data.STRef
  (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

import           MMZKDS.List as L (MList(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Queue (MDeque(..))
import           MMZKDS.Unboxed.Base (MULinkedList(..), MUNode(..))
import           MMZKDS.Unboxed.STURef
  (STU, STURef, modifySTURef, newSTURef, readSTURef, writeSTURef)
import           MMZKDS.Unsafe (unsafeSTEq)
import           MMZKDS.Utilities (outOfBoundError)


--------------------------------------------------------------------------------
-- List Instance
--------------------------------------------------------------------------------

instance STU a s => MList (MULinkedList a) a ST s where
  mDelete :: Int -> MULinkedList a s -> ST s (Maybe a)
  mDelete index mll@(MULinkedList lR _ iR cR) = do
    l <- readSTURef lR
    if index < 0 || index >= l
      then return Nothing
      else do
        let preDelete i l
              | index == i = do
                cur <- readSTRef cR
                prv <- prevUN cur
                nxt <- nextUN cur
                writeSTURef iR (-1)
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
                i   <- readSTURef iR
                when (i == index + 1) $ modifySTURef iR pred
                return (prv, cur, nxt)
        let del prv cur nxt = do
            writeSTRef (prevUNRef nxt) prv
            writeSTRef (nextUNRef prv) nxt
            Just <$> uNodeElem cur
        i <- readSTURef iR
        (pre, cur, nxt) <- preDelete i l
        writeSTURef lR $ l - 1
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
        if      isHead node
        then    return []
        else if uNodeElem node `unsafeSTEq` return e
        then do
          nxt <- nextUN node
          rst <- mIndicesOf' (i + 1) nxt
          return $ i : rst
        else    nextUN node >>= mIndicesOf' (i + 1)
    hd <- getHead mll
    nextUN hd >>= mIndicesOf' 0

  mInsert :: Int -> a -> MULinkedList a s -> ST s ()
  mInsert index e mll@(MULinkedList lR _ iR cR) = do
    l <- readSTURef lR
    if index < 0 || index > l
      then outOfBoundError index
      else do
        accessUNode index mll
        cur <- readSTRef cR
        prv <- prevUN cur
        nR  <- newSTRef cur
        pR  <- newSTRef prv
        eR  <- newSTURef e
        let newNode = MUNode pR eR nR
        writeSTRef (prevUNRef cur) newNode
        writeSTRef (nextUNRef prv) newNode
        writeSTRef cR newNode
        writeSTURef lR $ l + 1

  mSet :: MULinkedList a s -> Int -> a -> ST s ()
  mSet mll@(MULinkedList _ _ _ cR) index e = do
    accessUNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else let MUNode _ eR _ = cur in writeSTURef eR e

  mSortOn :: Ord b => (a -> b) -> MULinkedList a s -> ST s ()
  mSortOn f mll@(MULinkedList _ hR iR cR) = do
    i    <- readSTURef iR
    list <- mToList mll
    let list' = sortOn (f . snd) $ zip [0..] list
    let ui'   = elemIndex i (fst <$> list')
    mll' <- mNewList $ snd <$> list'
    hd   <- getHead mll'
    writeSTRef hR $! hd
    writeSTRef cR $! hd
    writeSTURef iR $ -1
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
    eR  <- newSTURef e
    let newNode = MUNode pR eR nR
    writeSTRef (prevUNRef cur) newNode
    writeSTRef (nextUNRef prv) newNode
    modifySTURef lR succ

  -- Overwritten default method
  mIndexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
  mIndexOf mll e = do
    let mIndexOf' i node
          | isHead node                          = return Nothing
          | uNodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise                            = nextUN node >>= 
                                                   mIndexOf' (i + 1)
    hd <- getHead mll
    nextUN hd >>= mIndexOf' 0

  -- Overwritten default method
  mLastIndexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
  mLastIndexOf mll e = do
    l <- size mll
    let mLastIndexOf' i node
          | isHead node                          = return Nothing
          | uNodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise                            = prevUN node >>= 
                                                   mLastIndexOf' (i - 1)
    hd <- getHead mll
    prevUN hd >>= mLastIndexOf' (l - 1)

  -- Overwritten default method
  mPush :: a -> MULinkedList a s -> ST s ()
  mPush e (MULinkedList lR hR iR _) = do
    cur <- readSTRef hR
    nxt <- nextUN cur
    pR  <- newSTRef cur
    nR  <- newSTRef nxt
    eR  <- newSTURef e
    let newNode = MUNode pR eR nR
    writeSTRef (nextUNRef cur) newNode
    writeSTRef (prevUNRef nxt) newNode
    modifySTURef lR succ
    i   <- readSTURef iR
    when (i >= 0) $ modifySTURef iR succ


--------------------------------------------------------------------------------
-- MDeque Instance
--------------------------------------------------------------------------------

instance STU a s => MDeque (MULinkedList a) a ST s where
  mDequeueFront :: MULinkedList a s -> ST s (Maybe a)
  mDequeueFront = mPopFront

  mDequeueEnd :: MULinkedList a s -> ST s (Maybe a)
  mDequeueEnd = mPop

  mEnqueueFront :: a -> MULinkedList a s -> ST s ()
  mEnqueueFront = mPush

  mEnqueueEnd :: a -> MULinkedList a s -> ST s ()
  mEnqueueEnd = mAppend


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance STU a s => MDS (MULinkedList a) ST s where
  clear :: MULinkedList a s -> ST s ()
  clear (MULinkedList lR hR iR cR) = do
    writeSTURef lR 0
    writeSTURef iR $ -1
    hd <- newHead
    writeSTRef hR hd
    writeSTRef cR hd

  copy :: MULinkedList a s -> ST s (MULinkedList a s)
  copy = new <=< mToList

  size :: MULinkedList a s -> ST s Int
  size (MULinkedList lR _ _ _)
    = readSTURef lR

instance STU a s => MDSCons [a] (MULinkedList a) ST s where
  finish :: MULinkedList a s -> ST s [a]
  finish mll = do
    let mToList' node = if isHead node
        then return []
        else liftM2 (:) (uNodeElem node) (nextUN node >>= mToList')
    getHead mll >>= nextUN >>= mToList'

  {-# INLINE new #-}
  new :: [a] -> ST s (MULinkedList a s)
  new xs = do
    mll <- emptyMULinkedList
    let MULinkedList lR hR _ _ = mll
    let len = length xs
    cur <- readSTRef hR
    let go e = do
            prv <- prevUN cur
            nR  <- newSTRef cur
            pR  <- newSTRef prv
            eR  <- newSTURef e
            let newNode = MUNode pR eR nR
            writeSTRef (prevUNRef cur) newNode
            writeSTRef (nextUNRef prv) newNode
    forM_ xs go
    writeSTURef lR len
    return mll


--------------------------------------------------------------------------------
-- Node-Specific Functions
--------------------------------------------------------------------------------

-- | Utility Function.
-- Set the reference to the current node at the given index.
-- If the index is out of bound, set it to head.
accessUNode :: Int -> MULinkedList a s -> ST s ()
accessUNode index (MULinkedList lR hR iR cR) = do
  l   <- readSTURef lR
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
  i   <- readSTURef iR
  nd' <- access' i l
  writeSTURef iR $ if inBound then index else -1
  writeSTRef cR nd'

-- | Utility Function.
-- Creates an empty linked list.
emptyMULinkedList :: ST s (MULinkedList a s)
emptyMULinkedList = do
  lR <- newSTURef 0
  iR <- newSTURef $ -1
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
uNodeElem :: STU a s => MUNode a s -> ST s a
uNodeElem (MUNode _ eR _)
  = readSTURef eR
