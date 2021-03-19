{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MHeapPQ where

import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST, runST)
import           Data.Array (Array)
import           Data.Array.ST
  (STArray, getBounds, freeze, newArray_, newListArray, thaw, readArray,
  writeArray
  )
import           Data.Foldable (toList)
import           Data.Maybe (fromJust, isNothing)
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import           MMZKDS.ArrayBased (MArrayBased(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.PriorityQueue (MPriorityQueue(..))
import           MMZKDS.Unboxed.MURef (MURef, newMURef, readMURef, writeMURef)
import           MMZKDS.Unsafe
  (unsafeCopyArray, unsafeLeftChild, unsafeParent, unsafeRightChild)
import           MMZKDS.Utilities
  (arrayLengthOverflowError, expandedSize, initialSize, outOfBoundError)

-- | 'MHeapPQ' is a min-heap implementing the 'MPriorityQueue' class.
-- The heap is implemented with an internal @STArray@.
-- It is expected that the type of its elements is an instance of 'Ord'.
-- It may adds an element to anywhere in the array, but it always pops the 
-- "smallest" element.
-- 
data MHeapPQ e s = MHeapPQ {
  mHeapS :: MURef s Int,
  mHeapA :: STRef s (STArray s Int e)
  }


--------------------------------------------------------------------------------
-- MPriorityQueue Instance
--------------------------------------------------------------------------------

instance Ord a => MPriorityQueue MHeapPQ a ST s where
  mAdd :: a -> MHeapPQ a s -> ST s ()
  mAdd e mh@(MHeapPQ lR arrR) = do
    ls <- size mh
    ps <- mPhysicalSize mh
    if ls == ps
      then mResize (expandedSize ls) mh >> mAdd e mh
      else do
        arrST <- readSTRef arrR
        writeMURef lR $! ls + 1
        writeArray arrST ls e
        let bubbleUp i = when (i > 0) $ do
            let pI = unsafeParent i
            vi <- readArray arrST i
            vp <- readArray arrST pI
            when (vi < vp) $ 
              writeArray arrST i vp >> writeArray arrST pI vi >> bubbleUp pI
        bubbleUp ls

  mPop :: MHeapPQ a s -> ST s (Maybe a)
  mPop mh@(MHeapPQ lR arrR) = do
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
        writeMURef lR lastI
        return $ Just popE


--------------------------------------------------------------------------------
-- MArrayBased Instances
--------------------------------------------------------------------------------

instance Ord a => MArrayBased MHeapPQ a ST s where
  mDeepClear :: MHeapPQ a s -> ST s ()
  mDeepClear mh =
    (newArray_ (0, initialSize 0 - 1) :: ST s (STArray s Int a)) >>=
      writeSTRef (mHeapA mh) >> writeMURef (mHeapS mh) 0

  mNewWithSize :: Foldable f => Int -> f a -> ST s (MHeapPQ a s)
  mNewWithSize s fd = do
    let l = length fd
    arrST <- (newListArray :: (Int, Int) -> [a] -> ST s (STArray s Int a))
      (0, max s $ initialSize l - 1) $ toList fd
    lR    <- newMURef l
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

  mPhysicalSize :: MHeapPQ a s -> ST s Int
  mPhysicalSize mh = do
    (_, sup) <- readSTRef (mHeapA mh) >>= getBounds
    return (sup + 1)

  mResize :: Int -> MHeapPQ a s -> ST s ()
  mResize s _
    | s < 0 = arrayLengthOverflowError
  mResize s (MHeapPQ lR arrR) = do
    arrST    <- readSTRef arrR
    l        <- readMURef lR
    (_, sup) <- getBounds arrST
    resST    <- newArray_ (0, max s (sup + 1) - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    writeSTRef arrR resST

  trueCopy :: MHeapPQ a s -> ST s (MHeapPQ a s)
  trueCopy mh = do
    l     <- readMURef (mHeapS mh)
    lR    <- newMURef l
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
  clear = flip writeMURef 0 . mHeapS

  copy :: MHeapPQ a s -> ST s (MHeapPQ a s)
  copy mh = do
    l     <- readMURef (mHeapS mh)
    lR    <- newMURef l
    arrST <- readSTRef (mHeapA mh)
    resST <- newArray_ (0, initialSize l - 1)
    unsafeCopyArray arrST resST (0, l - 1)
    aR    <- newSTRef resST
    return $ MHeapPQ lR aR

  size :: MHeapPQ a s -> ST s Int
  size = readMURef . mHeapS

instance Ord a => MDSCons [a] (MHeapPQ a) ST s where
  finish :: MHeapPQ a s -> ST s [a]
  finish mh = do
    let freeze' = freeze :: STArray s Int a -> ST s (Array Int a)
    readMURef (mHeapS mh) >>=
      flip fmap (readSTRef (mHeapA mh) >>= freeze') . (. toList) . take

  new :: [a] -> ST s (MHeapPQ a s)
  new = mNewWithSize 0


--------------------------------------------------------------------------------
-- Heap-Specific Functions
--------------------------------------------------------------------------------

-- | Unsafe function: Does not the validity of childrens.
-- Turns the heap into a min-heap starting from the given index.
-- Pre: The indices of childrens are valid.  
fixHead :: Ord a => STArray s Int a -> Int -> Int -> Maybe Int -> Maybe Int -> ST s ()
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


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

foo = runST $ do
  let n = 15
  mh <- new [n, (n - 1)..1 :: Int] :: ST s (MHeapPQ Int s)
  mAdd (-1) mh
  mAdd (-3) mh
  mAdd (-2) mh
  (finish :: MHeapPQ Int s -> ST s [Int]) mh