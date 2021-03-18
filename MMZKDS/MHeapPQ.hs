{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MHeapPQ where

import           Control.Monad (when)
import           Control.Monad.ST (ST, runST)
import           Data.Array (Array)
import           Data.Array.ST
  (STArray, getBounds, freeze, newArray_, newListArray, thaw, readArray,
  writeArray
  )
import           Data.Foldable (toList)
import           Data.Maybe (fromJust, isNothing)
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import           MMZKDS.ArrayBased (ArrayBased(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.PriorityQueue (MPriorityQueue(..))
import           MMZKDS.Unboxed.MURef (MURef, newMURef, readMURef, writeMURef)
import           MMZKDS.Unsafe
  (unsafeCopyArray, unsafeLeftChild, unsafeRightChild)
import           MMZKDS.Utilities (initialSize)

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
  mAdd = undefined
  mPop = undefined


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance Ord a => MDS (MHeapPQ a) ST s where
  clear :: MHeapPQ a s -> ST s ()
  clear = flip writeMURef 0 . mHeapS

  copy :: MHeapPQ a s -> ST s (MHeapPQ a s)
  copy mh = do
    lR    <- readMURef (mHeapS mh) >>= newMURef
    arrST <- readSTRef (mHeapA mh)
    bd    <- getBounds arrST
    resST <- newArray_ bd
    unsafeCopyArray arrST resST bd
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
  new list = do
    let l   = length list
    arrST <- (newListArray :: (Int, Int) -> [a] -> ST s (STArray s Int a)) 
      (0, initialSize l - 1) list
    lR    <- newMURef l
    aR    <- newSTRef arrST
    let glc = flip unsafeLeftChild l
    let grc = flip unsafeRightChild l
    let wt' = (writeArray :: STArray s Int a -> Int -> a -> ST s ()) arrST
    let fixHead i mlc mrc
          | isNothing mlc = return ()
          | isNothing mrc = do
            let Just lc = mlc
            vi  <- readArray arrST i
            vlc <- readArray arrST lc
            when (vi > vlc) $ 
              wt' i vlc >> wt' lc vi >> fixHead lc (glc lc) (grc lc)
          | otherwise     = do
            let Just lc = mlc
            let Just rc = mrc
            vi  <- readArray arrST i
            vlc <- readArray arrST lc
            vrc <- readArray arrST rc
            
            when (vi > vlc || vi > vrc) $ if vlc < vrc 
              then wt' i vlc >> wt' lc vi >> fixHead lc (glc lc) (grc lc)
              else wt' i vrc >> wt' rc vi >> fixHead rc (glc rc) (grc rc)
    let toMinHeap mi
          | isNothing mi = return Nothing
          | otherwise 
            = toMinHeap mlc >> toMinHeap mrc >> fixHead i mlc mrc >> return mi
          where
            i   = fromJust mi
            mlc = unsafeLeftChild i l
            mrc = unsafeRightChild i l
    toMinHeap $ Just 0
    return $ MHeapPQ lR aR

foo = runST $ do
  let n = 15
  mh <- new [n, (n - 1)..1 :: Int] :: ST s (MHeapPQ Int s)
  mi <- copy mh
  clear mh
  (finish :: MHeapPQ Int s -> ST s [Int]) mi
