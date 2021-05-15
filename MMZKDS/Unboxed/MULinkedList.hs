{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Unboxed.MULinkedList (MULinkedList) where

import           Control.Monad (forM, forM_, liftM2, when, (<=<))
import           Control.Monad.ST (ST)
import           Data.List as L (elemIndex, sortOn)
import           Data.Maybe (fromJust, isJust)
import           Data.STRef
  (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)

import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Class.MList (MList(..))
import           MMZKDS.Class.MQueue (MDeque(..))
import           MMZKDS.Unboxed.Base (MULinkedList(..), MUNode(..))
import           MMZKDS.Unboxed.STURef
  (STU, STURef, modifySTURef, newSTURef, readSTURef, writeSTURef)
import           MMZKDS.Unsafe (unsafeSTEq)
import           MMZKDS.Utilities (idMULinkedList, outOfBoundError)


--------------------------------------------------------------------------------
-- List Instance
--------------------------------------------------------------------------------

instance STU a s => MList (MULinkedList a) a ST s where
  delete :: Int -> MULinkedList a s -> ST s (Maybe a)
  delete index mll@(MULinkedList lR _ iR cR) = do
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

  get :: MULinkedList a s -> Int -> ST s a
  get mll@(MULinkedList _ _ _ cR) index = do
    accessUNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else uNodeElem cur

  indicesOf :: Eq a => MULinkedList a s -> a -> ST s [Int]
  indicesOf mll e = do
    let indicesOf' i node = do
        if      isHead node
        then    return []
        else if uNodeElem node `unsafeSTEq` return e
        then do
          nxt <- nextUN node
          rst <- indicesOf' (i + 1) nxt
          return $ i : rst
        else    nextUN node >>= indicesOf' (i + 1)
    hd <- getHead mll
    nextUN hd >>= indicesOf' 0

  insert :: Int -> a -> MULinkedList a s -> ST s ()
  insert index e mll@(MULinkedList lR _ iR cR) = do
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

  set :: MULinkedList a s -> Int -> a -> ST s ()
  set mll@(MULinkedList _ _ _ cR) index e = do
    accessUNode index mll
    cur <- readSTRef cR
    if isHead cur
      then outOfBoundError index
      else let MUNode _ eR _ = cur in writeSTURef eR e

  sortOn :: Ord b => (a -> b) -> MULinkedList a s -> ST s ()
  sortOn f mll@(MULinkedList _ hR iR cR) = do
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
      Just i' -> accessUNode i' mll

  subList :: Int -> Int -> MULinkedList a s -> ST s (MULinkedList a s)
  subList inf sup mll@(MULinkedList _ _ _ cR) = do
    ls <- size mll
    forM [(max inf 0)..(min sup ls - 1)]
      ((>> (readSTRef cR >>= uNodeElem)) . flip accessUNode mll) >>= newList

  -- Overwritten default method
  append :: a -> MULinkedList a s -> ST s ()
  append e (MULinkedList lR hR _ _) = do
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
  deleteRange :: Int -> Int -> MULinkedList a s -> ST s [a]
  deleteRange inf sup mll@(MULinkedList lR hR iR cR) = do
    ls    <- size mll
    index <- readSTURef iR
    cur   <- readSTRef cR
    let inf' = max 0 inf
    let sup' = min ls sup
    res   <- subList inf sup mll >>= toList
    sR    <- if inf' == 0 
          then return hR 
          else accessUNode (inf' - 1) mll >> return cR
    start <- readSTRef sR
    eR    <- if sup' == ls
          then return hR 
          else accessUNode sup' mll >> return cR
    end   <- readSTRef eR
    writeSTRef (nextUNRef start) end
    writeSTRef (prevUNRef end) start
    writeSTURef lR (ls - (sup' - inf'))
    if      index >= inf' && index < sup'
    then    writeSTURef iR (-1) >> readSTRef hR >>= writeSTRef cR
    else if index < inf' 
    then     writeSTURef iR index >> writeSTRef cR cur
    else    writeSTURef iR (index - (sup' - inf')) >> writeSTRef cR cur
    return res

  -- Overwritten default method
  indexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
  indexOf mll e = do
    let indexOf' i node
          | isHead node                          = return Nothing
          | uNodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise                            = nextUN node >>= 
                                                   indexOf' (i + 1)
    hd <- getHead mll
    nextUN hd >>= indexOf' 0

  -- Overwritten default method
  lastIndexOf :: Eq a => MULinkedList a s -> a -> ST s (Maybe Int)
  lastIndexOf mll e = do
    l <- size mll
    let lastIndexOf' i node
          | isHead node                          = return Nothing
          | uNodeElem node `unsafeSTEq` return e = return $ Just i
          | otherwise                            = prevUN node >>= 
                                                   lastIndexOf' (i - 1)
    hd <- getHead mll
    prevUN hd >>= lastIndexOf' (l - 1)

  -- Overwritten default method
  push :: a -> MULinkedList a s -> ST s ()
  push e (MULinkedList lR hR iR _) = do
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
  dequeueFront :: MULinkedList a s -> ST s (Maybe a)
  dequeueFront = popFront

  dequeueEnd :: MULinkedList a s -> ST s (Maybe a)
  dequeueEnd = pop

  enqueueFront :: a -> MULinkedList a s -> ST s ()
  enqueueFront = push

  enqueueEnd :: a -> MULinkedList a s -> ST s ()
  enqueueEnd = append


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
  copy = new <=< toList

  identifier :: MULinkedList a s -> ST s String
  identifier = const $ return idMULinkedList 

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
