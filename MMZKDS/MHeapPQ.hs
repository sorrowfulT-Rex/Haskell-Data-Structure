{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MHeapPQ (MHeapPQ) where

import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST)
import           Data.Array (Array)
import           Data.Array.ST
  ( STArray, getBounds, freeze, newArray_, newListArray, thaw, readArray
  , writeArray
  )
import           Data.Bool (bool)
import           Data.Foldable (toList)
import           Data.Maybe (fromJust, isNothing)
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import           MMZKDS.Class.MArrayBased (MArrayBased(..))
import           MMZKDS.Base (MHeapPQ(..))
import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Class.MPriorityQueue (MPriorityQueue(..))
import           MMZKDS.Unboxed.STURef 
  (STURef, newSTURef, readSTURef, writeSTURef)
import           MMZKDS.Unsafe
  (unsafeCopyArray, unsafeLeftChild, unsafeParent, unsafeRightChild)
import           MMZKDS.Utilities
  ( arrayLengthOverflowError, expandedSize, idMHeapPQ, initialSize
  , outOfBoundError
  )


--------------------------------------------------------------------------------
-- MPriorityQueue Instance
--------------------------------------------------------------------------------

instance Ord a => MPriorityQueue (MHeapPQ a) a ST s where
  {-# INLINE add #-}
  add :: MHeapPQ a s -> a -> ST s ()
  add mh@(MHeapPQ lR arrR) e = do
    ls <- size mh
    ps <- physicalSize mh
    if ls == ps
      then resize (expandedSize ls) mh >> add mh e
      else do
        arrST <- readSTRef arrR
        writeSTURef lR $! ls + 1
        writeArray arrST ls e
        let bubbleUp i = when (i > 0) $ do
            let pI = unsafeParent i
            vi <- readArray arrST i
            vp <- readArray arrST pI
            when (vi < vp) $
              writeArray arrST i vp >> writeArray arrST pI vi >> bubbleUp pI
        bubbleUp ls

  {-# INLINE pop #-}
  pop :: MHeapPQ a s -> ST s (Maybe a)
  pop mh@(MHeapPQ lR arrR) = do
    l <- size mh
    if l == 0
      then return Nothing
      else do
        let lastI = l - 1
        let glc   = flip unsafeLeftChild lastI
        let grc   = flip unsafeRightChild lastI
        arrST <- readSTRef arrR
        popE  <- readArray arrST 0
        lastE <- readArray arrST lastI
        writeArray arrST 0 lastE
        writeArray arrST lastI popE
        fixHead arrST lastI 0 (glc 0) (grc 0)
        writeSTURef lR lastI
        return $ Just popE

  -- Overwritten default method
  peek :: MHeapPQ a s -> ST s (Maybe a)
  peek mh@(MHeapPQ _ arrR)
    = isNull mh >>= 
      bool (fmap Just $ readSTRef arrR >>= flip readArray 0) (return Nothing)


--------------------------------------------------------------------------------
-- MArrayBased Instance
--------------------------------------------------------------------------------

instance Ord a => MArrayBased (MHeapPQ a) a ST s where
  deepClear :: MHeapPQ a s -> ST s ()
  deepClear mh =
    (newArray_ (0, initialSize 0 - 1) :: ST s (STArray s Int a)) >>=
      writeSTRef (mHeapA mh) >> writeSTURef (mHeapS mh) 0

  {-# INLINE newWithSize #-}
  newWithSize :: Foldable f => Int -> f a -> ST s (MHeapPQ a s)
  newWithSize s fd = do
    let l = length fd
    arrST <- (newListArray :: (Int, Int) -> [a] -> ST s (STArray s Int a))
      (0, max s (initialSize l) - 1) $ toList fd
    lR    <- newSTURef l
    aR    <- newSTRef arrST
    let toMinHeap mi
          | isNothing mi = return Nothing
          | otherwise    = toMinHeap mlc >> toMinHeap mrc >>=
            fixHead arrST l i mlc >> return mi
          where
            i   = fromJust mi
            mlc = unsafeLeftChild i l
            mrc = unsafeRightChild i l
    toMinHeap $ Just 0
    return $ MHeapPQ lR aR

  physicalSize :: MHeapPQ a s -> ST s Int
  physicalSize mh = do
    (_, sup) <- readSTRef (mHeapA mh) >>= getBounds
    return (sup + 1)

  resize :: Int -> MHeapPQ a s -> ST s ()
  resize s _
    | s < 0 = arrayLengthOverflowError
  resize s (MHeapPQ lR arrR) = do
    arrST    <- readSTRef arrR
    l        <- readSTURef lR
    (_, sup) <- getBounds arrST
    resST    <- newArray_ (0, max s (sup + 1) - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    writeSTRef arrR resST

  trueCopy :: MHeapPQ a s -> ST s (MHeapPQ a s)
  trueCopy mh = do
    l     <- readSTURef (mHeapS mh)
    lR    <- newSTURef l
    arrST <- readSTRef (mHeapA mh)
    resST <- getBounds arrST >>= newArray_
    unsafeCopyArray arrST resST (0, l - 1)
    aR    <- newSTRef resST
    return $ MHeapPQ lR aR


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance Ord a => MDS (MHeapPQ a) ST s where
  clear :: MHeapPQ a s -> ST s ()
  clear = flip writeSTURef 0 . mHeapS

  copy :: MHeapPQ a s -> ST s (MHeapPQ a s)
  copy mh = do
    l     <- readSTURef (mHeapS mh)
    lR    <- newSTURef l
    arrST <- readSTRef (mHeapA mh)
    resST <- newArray_ (0, initialSize l - 1)
    unsafeCopyArray arrST resST (0, l - 1)
    aR    <- newSTRef resST
    return $ MHeapPQ lR aR

  identifier :: MHeapPQ a s -> ST s String
  identifier = const $ return idMHeapPQ

  size :: MHeapPQ a s -> ST s Int
  size = readSTURef . mHeapS

instance Ord a => MDSCons [a] (MHeapPQ a) ST s where
  finish :: MHeapPQ a s -> ST s [a]
  finish mh = do
    let freeze' = freeze :: STArray s Int a -> ST s (Array Int a)
    readSTURef (mHeapS mh) >>=
      flip fmap (readSTRef (mHeapA mh) >>= freeze') . (. toList) . take

  new :: [a] -> ST s (MHeapPQ a s)
  new xs = newWithSize (initialSize $ length xs) xs


--------------------------------------------------------------------------------
-- Heap-Specific Function
--------------------------------------------------------------------------------

-- | Unsafe function: Does not check the validity of childrens.
-- Turns the heap into a min-heap starting from the given index.
-- Pre: The indices of childrens are valid. 
--  
{-# INLINE fixHead #-}
fixHead :: Ord a
        => STArray s Int a -- ^ The @STArray@
        -> Int -- ^ The logic length
        -> Int -- ^ The index
        -> Maybe Int -- ^ The index of the left child
        -> Maybe Int -- ^ The index of the right child
        -> ST s ()
fixHead arrST l i mlc mrc = do
  let glc = flip unsafeLeftChild l
  let grc = flip unsafeRightChild l
  let wt' = (writeArray :: STArray s Int a -> Int -> a -> ST s ()) arrST
  let fixHead' i mlc mrc
          | isNothing mlc = return ()
          | isNothing mrc = do
            let Just lc = mlc
            vi  <- readArray arrST i
            vlc <- readArray arrST lc
            when (vi > vlc) $
              wt' i vlc >> wt' lc vi >> fixHead' lc (glc lc) (grc lc)
          | otherwise     = do
            let Just lc = mlc
            let Just rc = mrc
            vi  <- readArray arrST i
            vlc <- readArray arrST lc
            vrc <- readArray arrST rc
            when (vi > vlc || vi > vrc) $ if vlc < vrc
              then wt' i vlc >> wt' lc vi >> fixHead' lc (glc lc) (grc lc)
              else wt' i vrc >> wt' rc vi >> fixHead' rc (glc rc) (grc rc)
  fixHead' i mlc mrc
