{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Unboxed.MUHeapPQ (MUHeapPQ) where

import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST)
import           Data.Array.ST
  (STUArray, getBounds, freeze, newArray_, newListArray, thaw, readArray,
  writeArray
  )
import           Data.Array.Unboxed (IArray, UArray, (!))
import           Data.Bool (bool)
import           Data.Foldable (toList)
import           Data.Maybe (fromJust, isNothing)
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import           MMZKDS.ArrayBased (MArrayBased(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.PriorityQueue (MPriorityQueue(..))
import           MMZKDS.Unboxed.Base (MUHeapPQ(..))
import           MMZKDS.Unboxed.MURef 
  (MU, MURef, newMURef, readMURef, writeMURef)
import           MMZKDS.Unsafe
  (unsafeCopyArray, unsafeLeftChild, unsafeParent, unsafeRightChild)
import           MMZKDS.Utilities
  (arrayLengthOverflowError, expandedSize, initialSize, outOfBoundError)


--------------------------------------------------------------------------------
-- MPriorityQueue Instance
--------------------------------------------------------------------------------

instance (Ord a, IArray UArray a, MU a s) 
  => MPriorityQueue MUHeapPQ a ST s where
  mAdd :: a -> MUHeapPQ a s -> ST s ()
  mAdd e mh@(MUHeapPQ lR arrR) = do
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

  mPop :: MUHeapPQ a s -> ST s (Maybe a)
  mPop mh@(MUHeapPQ lR arrR) = do
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

  -- Overwritten default method
  mPeek :: MUHeapPQ a s -> ST s (Maybe a)
  mPeek mh@(MUHeapPQ _ arrR)
    = isNull mh >>= 
      bool (fmap Just $ readSTRef arrR >>= flip readArray 0) (return Nothing)


--------------------------------------------------------------------------------
-- MArrayBased Instance
--------------------------------------------------------------------------------

instance (Ord a, IArray UArray a, MU a s) => MArrayBased MUHeapPQ a ST s where
  mDeepClear :: MUHeapPQ a s -> ST s ()
  mDeepClear mh =
    (newArray_  :: MU a s 
                => (Int, Int) 
                -> ST s (STUArray s Int a)
    ) (0, initialSize 0 - 1) >>=
      writeSTRef (mHeapA mh) >> writeMURef (mHeapS mh) 0

  mNewWithSize :: Foldable f => Int -> f a -> ST s (MUHeapPQ a s)
  mNewWithSize s fd = do
    let l = length fd
    arrST <- (newListArray :: MU a s 
                           => (Int, Int) 
                           -> [a] 
                           -> ST s (STUArray s Int a)
             ) (0, max s $ initialSize l - 1) $ toList fd
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
    return $ MUHeapPQ lR aR

  mPhysicalSize :: MUHeapPQ a s -> ST s Int
  mPhysicalSize mh = do
    (_, sup) <- readSTRef (mHeapA mh) >>= getBounds
    return (sup + 1)

  mResize :: Int -> MUHeapPQ a s -> ST s ()
  mResize s _
    | s < 0 = arrayLengthOverflowError
  mResize s (MUHeapPQ lR arrR) = do
    arrST    <- readSTRef arrR
    l        <- readMURef lR
    (_, sup) <- getBounds arrST
    resST    <- newArray_ (0, max s (sup + 1) - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    writeSTRef arrR resST

  trueCopy :: MUHeapPQ a s -> ST s (MUHeapPQ a s)
  trueCopy mh = do
    l     <- readMURef (mHeapS mh)
    lR    <- newMURef l
    arrST <- readSTRef (mHeapA mh)
    resST <- getBounds arrST >>= newArray_
    unsafeCopyArray arrST resST (0, l - 1)
    aR    <- newSTRef resST
    return $ MUHeapPQ lR aR


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance (Ord a, MU a s) => MDS (MUHeapPQ a) ST s where
  clear :: MUHeapPQ a s -> ST s ()
  clear = flip writeMURef 0 . mHeapS

  copy :: MUHeapPQ a s -> ST s (MUHeapPQ a s)
  copy mh = do
    l     <- readMURef (mHeapS mh)
    lR    <- newMURef l
    arrST <- readSTRef (mHeapA mh)
    resST <- newArray_ (0, initialSize l - 1)
    unsafeCopyArray arrST resST (0, l - 1)
    aR    <- newSTRef resST
    return $ MUHeapPQ lR aR

  size :: MUHeapPQ a s -> ST s Int
  size = readMURef . mHeapS

instance (Ord a, IArray UArray a, MU a s) => MDSCons [a] (MUHeapPQ a) ST s where
  finish :: MUHeapPQ a s -> ST s [a]
  finish mh@(MUHeapPQ _ arrR) = do
    l <- size mh
    arrST <- readSTRef arrR
    let freeze' = freeze :: (IArray UArray a, MU a s) 
                         => STUArray s Int a 
                         -> ST s (UArray Int a)
    arr   <- freeze' arrST
    return $ take l $ toList' arr 0 l
    where
      toList' arr i l
        | i == l    = []
        | otherwise = (arr ! i) : toList' arr (i + 1) l

  new :: [a] -> ST s (MUHeapPQ a s)
  new = mNewWithSize 0


--------------------------------------------------------------------------------
-- Heap-Specific Function
--------------------------------------------------------------------------------

-- | Unsafe function: Does not the validity of childrens.
-- Turns the heap into a min-heap starting from the given index.
-- Pre: The indices of childrens are valid.  
fixHead :: (Ord a, MU a s)
        => STUArray s Int a -- ^ The @STUArray@
        -> Int -- ^ The logic length
        -> Int -- ^ The index
        -> Maybe Int -- ^ The index of the left child
        -> Maybe Int -- ^ The index of the right child
        -> ST s ()
fixHead arrST l i mlc mrc = do
  let glc = flip unsafeLeftChild l
  let grc = flip unsafeRightChild l
  let wt' = (writeArray :: MU a s 
                        => STUArray s Int a 
                        -> Int 
                        -> a 
                        -> ST s ()
            ) arrST
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
