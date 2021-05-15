{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.MLinkedList (MLinkedList) where

import           Control.Monad (forM, forM_, liftM2, when, (<=<))
import           Control.Monad.ST (ST)
import           Data.List as L (elemIndex, sortOn)
import           Data.Maybe (fromJust, isJust)
import           Data.STRef
  (STRef, newSTRef, readSTRef, writeSTRef)

import           MMZKDS.Base (MLinkedList(..), MNode(..))
import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Class.MList (MList(..))
import           MMZKDS.Queue (MDeque(..))
import           MMZKDS.Unboxed.STURef
  (STURef, modifySTURef, newSTURef, readSTURef, writeSTURef)
import           MMZKDS.Unsafe (unsafeSTEq)
import           MMZKDS.Utilities (idMLinkedList, outOfBoundError)


--------------------------------------------------------------------------------
-- MList Instance
--------------------------------------------------------------------------------

instance MList (MLinkedList a) a ST s where
  delete :: Int -> MLinkedList a s -> ST s (Maybe a)
  delete index mll@(MLinkedList lR _ iR cR) = do
    l <- readSTURef lR
    if index < 0 || index >= l
      then return Nothing
      else do
        let preDelete i l
              | index == i = do
                cur <- readSTRef cR
                prv <- prevN cur
                nxt <- nextN cur
                writeSTURef iR (-1)
                getHead mll >>= writeSTRef cR
                return (prv, cur, nxt)
              | i < index  = do
                accessNode (index - 1) mll
                prv <- readSTRef cR
                cur <- nextN prv
                nxt <- nextN cur
                return (prv, cur, nxt)
              | otherwise  = do
                accessNode (index + 1) mll
                nxt <- readSTRef cR
                cur <- prevN nxt
                prv <- prevN cur
                i   <- readSTURef iR
                when (i == index + 1) $ modifySTURef iR pred
                return (prv, cur, nxt)
        let del prv cur nxt = do
            writeSTRef (prevNRef nxt) prv
            writeSTRef (nextNRef prv) nxt
            Just <$> nodeElem cur
        i <- readSTURef iR
        (pre, cur, nxt) <- preDelete i l
        writeSTURef lR $! l - 1
        del pre cur nxt

  get :: MLinkedList a s -> Int -> ST s a
  get mll@(MLinkedList _ _ _ cR) index = do
    accessNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else nodeElem cur

  indicesOf :: Eq a => MLinkedList a s -> a -> ST s [Int]
  indicesOf mll e = do
    let mIndicesOf' i node = do
        if      isHead node
        then    return []
        else if nodeElem node `unsafeSTEq` return e
        then do
          nxt <- nextN node
          rst <- mIndicesOf' (i + 1) nxt
          return $ i : rst
        else    nextN node >>= mIndicesOf' (i + 1)
    hd <- getHead mll
    nextN hd >>= mIndicesOf' 0

  insert :: Int -> a -> MLinkedList a s -> ST s ()
  insert index e mll@(MLinkedList lR _ iR cR) = do
    l <- readSTURef lR
    if index < 0 || index > l
      then outOfBoundError index
      else do
        accessNode index mll
        i <- readSTURef iR
        cur <- readSTRef cR
        prv <- prevN cur
        nR  <- newSTRef cur
        pR  <- newSTRef prv
        eR  <- newSTRef e
        let newNode = MNode pR eR nR
        writeSTRef (prevNRef cur) newNode
        writeSTRef (nextNRef prv) newNode
        writeSTRef cR newNode
        writeSTURef lR $! l + 1

  set :: MLinkedList a s -> Int -> a -> ST s ()
  set mll@(MLinkedList _ _ _ cR) index e = do
    accessNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else let MNode _ eR _ = cur in writeSTRef eR e

  sortOn :: Ord b => (a -> b) -> MLinkedList a s -> ST s ()
  sortOn f mll@(MLinkedList _ hR iR cR) = do
    i    <- readSTURef iR
    list <- toList mll
    let list' = L.sortOn (f . snd) $ zip [0..] list
    let ui'   = elemIndex i (fst <$> list')
    mll' <- newList $ snd <$> list'
    hd   <- getHead mll'
    writeSTRef hR $! hd
    writeSTRef cR $! hd
    writeSTURef iR $ -1
    case ui' of
      Nothing -> return ()
      Just i' -> accessNode i' mll

  subList :: Int -> Int -> MLinkedList a s -> ST s (MLinkedList a s)
  subList inf sup mll@(MLinkedList _ _ _ cR) = do
    ls <- size mll
    forM [(max inf 0)..(min sup ls - 1)]
      ((>> (readSTRef cR >>= nodeElem)) . flip accessNode mll) >>= newList

  -- Overwritten default method
  append :: a -> MLinkedList a s -> ST s ()
  append e (MLinkedList lR hR _ _) = do
    cur <- readSTRef hR
    prv <- prevN cur
    nR  <- newSTRef cur
    pR  <- newSTRef prv
    eR  <- newSTRef e
    let newNode = MNode pR eR nR
    writeSTRef (prevNRef cur) newNode
    writeSTRef (nextNRef prv) newNode
    modifySTURef lR succ

  -- Overwritten default method
  deleteRange :: Int -> Int -> MLinkedList a s -> ST s [a]
  deleteRange inf sup mll@(MLinkedList lR hR iR cR) = do
    ls    <- size mll
    index <- readSTURef iR
    cur   <- readSTRef cR
    let inf' = max 0 inf
    let sup' = min ls sup
    res   <- subList inf sup mll >>= toList
    sR    <- if inf' == 0 
          then return hR 
          else accessNode (inf' - 1) mll >> return cR
    start <- readSTRef sR
    eR    <- if sup' == ls
          then return hR 
          else accessNode sup' mll >> return cR
    end   <- readSTRef eR
    writeSTRef (nextNRef start) end
    writeSTRef (prevNRef end) start
    writeSTURef lR (ls - (sup' - inf'))
    if      index >= inf' && index < sup'
    then    writeSTURef iR (-1) >> readSTRef hR >>= writeSTRef cR
    else if index < inf' 
    then     writeSTURef iR index >> writeSTRef cR cur
    else    writeSTURef iR (index - (sup' - inf')) >> writeSTRef cR cur
    return res

  -- Overwritten default method
  indexOf :: Eq a => MLinkedList a s -> a -> ST s (Maybe Int)
  indexOf mll e = do
    let mIndexOf' i node
          | isHead node                         = return Nothing
          | nodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise                           = nextN node >>= 
                                                  mIndexOf' (i + 1)
    hd <- getHead mll
    nextN hd >>= mIndexOf' 0

  -- Overwritten default method
  lastIndexOf :: Eq a => MLinkedList a s -> a -> ST s (Maybe Int)
  lastIndexOf mll e = do
    l <- size mll
    let lastIndexOf' i node
          | isHead node                         = return Nothing
          | nodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise                           = prevN node >>= 
                                                  lastIndexOf' (i - 1)
    hd <- getHead mll
    prevN hd >>= lastIndexOf' (l - 1)

  -- Overwritten default method
  push :: a -> MLinkedList a s -> ST s ()
  push e (MLinkedList lR hR iR _) = do
    cur <- readSTRef hR
    nxt <- nextN cur
    pR  <- newSTRef cur
    nR  <- newSTRef nxt
    eR  <- newSTRef e
    let newNode = MNode pR eR nR
    writeSTRef (nextNRef cur) newNode
    writeSTRef (prevNRef nxt) newNode
    modifySTURef lR succ
    i   <- readSTURef iR
    when (i >= 0) $ modifySTURef iR succ


--------------------------------------------------------------------------------
-- MDeque Instance
--------------------------------------------------------------------------------

instance MDeque (MLinkedList a) a ST s where
  mDequeueFront :: MLinkedList a s -> ST s (Maybe a)
  mDequeueFront = popFront

  mDequeueEnd :: MLinkedList a s -> ST s (Maybe a)
  mDequeueEnd = pop

  mEnqueueFront :: a -> MLinkedList a s -> ST s ()
  mEnqueueFront = push

  mEnqueueEnd :: a -> MLinkedList a s -> ST s ()
  mEnqueueEnd = append


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance MDS (MLinkedList a) ST s where
  clear :: MLinkedList a s -> ST s ()
  clear (MLinkedList lR hR iR cR) = do
    writeSTURef lR 0
    writeSTURef iR $ -1
    hd <- newHead
    writeSTRef hR hd
    writeSTRef cR hd

  copy :: MLinkedList a s -> ST s (MLinkedList a s)
  copy = new <=< toList

  identifier :: MLinkedList a s -> ST s String
  identifier = const $ return idMLinkedList 

  size :: MLinkedList a s -> ST s Int
  size (MLinkedList lR _ _ _)
    = readSTURef lR

instance MDSCons [a] (MLinkedList a) ST s where
  finish :: MLinkedList a s -> ST s [a]
  finish mll = do
    let mToList' node = if isHead node
        then return []
        else liftM2 (:) (nodeElem node) (nextN node >>= mToList')
    getHead mll >>= nextN >>= mToList'

  {-# INLINE new #-}
  new :: [a] -> ST s (MLinkedList a s)
  new xs = do
    mll <- emptyMLinkedList
    let MLinkedList lR hR _ _ = mll
    let len = length xs
    cur <- readSTRef hR
    let go e = do
            prv <- prevN cur
            nR  <- newSTRef cur
            pR  <- newSTRef prv
            eR  <- newSTRef e
            let newNode = MNode pR eR nR
            writeSTRef (prevNRef cur) newNode
            writeSTRef (nextNRef prv) newNode
    forM_ xs go
    writeSTURef lR len
    return mll


--------------------------------------------------------------------------------
-- Node-Specific Functions
--------------------------------------------------------------------------------

-- | Utility Function.
-- Set the reference to the current node at the given index.
-- If the index is out of bound, set it to head.
accessNode :: Int -> MLinkedList a s -> ST s ()
accessNode index (MLinkedList lR hR iR cR) = do
  l   <- readSTURef lR
  let inBound = index >= 0 && index < l
  let front' i nR
        | i == 0    = readSTRef nR
        | otherwise = readSTRef nR >>= front' (i - 1) . nextNRef
  let back' i nR
        | i == 0    = readSTRef nR
        | otherwise = readSTRef nR >>= back' (i - 1) . prevNRef
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
emptyMLinkedList :: ST s (MLinkedList a s)
emptyMLinkedList = do
  lR <- newSTURef 0
  iR <- newSTURef $ -1
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

-- | Unsafe: Does not check if the node is head.
-- Get the element from an @MNode@.
-- Pre: The node is not head.
nodeElem :: MNode a s -> ST s a
nodeElem (MNode _ eR _)
  = readSTRef eR

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
