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

-- | @MLinkedList@ is a doubly-linked circular list implementing the 'MList'
--  class.
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
  newMList :: Foldable f => f a -> ST s (MLinkedList a s)
  newMList xs = do
    mll <- emptyMLinkedList
    forM_ xs (flip mAppend mll)
    return mll

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

-- instance MDT (MLinkedList a) s where
--   copy :: MLinkedList a s -> ST s (MLinkedList a s)
--   copy (MLinkedList lR hR cR) = do
--     l     <- readSTRef lR
--     arrST <- readSTRef arrR
--     resST <- newArray_ (0, initialSize l - 1)
--     unsafeCopyArray arrST resST (0, l - 1)
--     rlR   <- newSTRef l
--     resR  <- newSTRef resST
--     return $ MArrayList rlR resR

instance Foldable f => MDTCons (f a) (MLinkedList a) s where
  new :: f a -> ST s (MLinkedList a s)
  new = newMList


--------------------------------------------------------------------------------
-- Node-Specific Functions
--------------------------------------------------------------------------------

emptyMLinkedList :: ST s (MLinkedList e s)
emptyMLinkedList = do
  lR <- newSTRef 0
  iR <- newSTRef 0
  pR <- newSTRef undefined
  nR <- newSTRef undefined
  let hNode = MHead pR nR
  writeSTRef pR hNode
  writeSTRef nR hNode
  hd <- newSTRef hNode
  return $ MLinkedList lR hd iR hd

isHead :: MNode e s -> Bool
isHead (MHead _ _)
  = True
isHead _
  = False

nextN :: MNode e s -> ST s (MNode e s)
nextN = readSTRef . nextNRef

nextNRef :: MNode e s -> STRef s (MNode e s)
nextNRef (MHead _ nR)
  = nR
nextNRef (MNode _ _ nR)
  = nR

prevN :: MNode e s -> ST s (MNode e s)
prevN = readSTRef . prevNRef

prevNRef :: MNode e s -> STRef s (MNode e s)
prevNRef (MHead pR _)
  = pR
prevNRef (MNode pR _ _)
  = pR

nodeElem :: MNode e s -> e
nodeElem (MNode _ e _)
  = e


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

bar = runST $ do
  e <- newMList [1,1,4,5,1,4] :: ST s (MLinkedList Int s)
  mToList e
