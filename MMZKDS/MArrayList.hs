{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.MArrayList where

import           Control.Monad (forM_, liftM2, when)
import           Control.Monad.ST (ST)
import           Data.Array.ST
  (STArray, freeze, getBounds, newArray_, readArray, thaw, writeArray)
import           Data.Array.Unsafe (unsafeFreeze, unsafeThaw)
import           Data.Maybe (fromJust, isJust)
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import           MMZKDS.ArrayBased (ArrayBased(..), MArrayBased(..))
import           MMZKDS.ArrayList (ArrayList(..))
import           MMZKDS.List as L (List(newList, toList), MList(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Queue (MQueue(..))
import           MMZKDS.Unboxed.MURef
  (MURef, newMURef, readMURef, writeMURef)
import           MMZKDS.Unsafe
  (unsafeAddST, unsafeCopyArray, unsafeQuickSort, unsafeRemoveST)
import           MMZKDS.Utilities
  (arrayLengthOverflowError, expandedSize, initialSize, outOfBoundError)

-- | @MArrayList@ is a data structure implementing the 'MList' class with an
-- internal @STArray@.
-- It has O(1) random access, O(1) appending/popping, O(n) inserting/deleting,
-- O(n) searching, and O(n * log n) sorting.
--
data MArrayList e s = MArrayList (MURef s Int) (STRef s (STArray s Int e))


--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

-- | Makes a immutable @ArrayList@ from a mutable @MArrayList@ by copying. 
--
arrayListFreeze :: MArrayList a s -> ST s (ArrayList a)
arrayListFreeze (MArrayList lR arrR) = do
  l     <- readMURef lR
  arrST <- readSTRef arrR
  arr   <- freeze arrST
  return $ ArrayList l arr

-- | Makes a mutable @MArrayList@ from an immutable @ArrayList@ by copying. 
--
arrayListThaw :: ArrayList a -> ST s (MArrayList a s)
arrayListThaw (ArrayList l arr) = do
  arrST <- thaw arr
  lR    <- newMURef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR

-- | Unsafe Function.
-- Makes an immutable @ArrayList@ from a mutable @MArrayList@, perhaps without
-- copying.
-- The original mutable list should not be used ever since.
--
unsafeArrayListFreeze :: MArrayList a s -> ST s (ArrayList a)
unsafeArrayListFreeze (MArrayList lR arrR) = do
  l     <- readMURef lR
  arrST <- readSTRef arrR
  arr   <- unsafeFreeze arrST
  return $ ArrayList l arr

-- | Unsafe Function.
-- Makes a mutable @MArrayList@ from an immutable @ArrayList@, perhaps without
-- copying.
-- The original immutable list should not be used ever since.
--
unsafeArrayListThaw :: ArrayList a -> ST s (MArrayList a s)
unsafeArrayListThaw (ArrayList l arr) = do
  arrST <- unsafeThaw arr
  lR    <- newMURef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR


--------------------------------------------------------------------------------
-- MList Instance
--------------------------------------------------------------------------------

instance MList MArrayList a ST s where
  mGet :: MArrayList a s -> Int -> ST s a
  mGet mal@(MArrayList lR arrR) index = do
    l <- size mal
    if index >= l || index < 0
      then outOfBoundError index
      else readSTRef arrR >>= flip readArray index

  mIndicesOf :: Eq a => MArrayList a s -> a -> ST s [Int]
  mIndicesOf mal e = size mal >>= mIndicesOf' 0
    where
      mIndicesOf' i l
        | i >= l = return []
      mIndicesOf' i l = do
        v <- mal `mGet` i
        if v == e
          then fmap (i :) (mIndicesOf' (i + 1) l)
          else mIndicesOf' (i + 1) l

  mInsert :: Int -> a -> MArrayList a s -> ST s ()
  mInsert index e mal@(MArrayList lR arrR) = do
    ls <- size mal
    ps <- mPhysicalSize mal
    if index < 0 || index > ls
      then outOfBoundError index
      else if ls == ps
        then do
          mResize (expandedSize ls) mal
          mInsert index e mal
        else do
          arrST <- readSTRef arrR
          writeMURef lR $! ls + 1
          unsafeAddST index e (ls - 1) arrST

  mDelete :: Int -> MArrayList a s -> ST s (Maybe a)
  mDelete index mal@(MArrayList lR arrR) = do
    ls <- size mal
    ps <- mPhysicalSize mal
    if index < 0 || index >= ls
      then return Nothing
      else do
        arrST <- readSTRef arrR
        v     <- readArray arrST index
        writeMURef lR $! ls - 1
        unsafeRemoveST index (ls - 2) arrST
        return $ Just v

  mSet :: MArrayList a s -> Int -> a -> ST s ()
  mSet mal@(MArrayList _ arrR) index e = do
    ls <- size mal
    if index < 0 || index >= ls
      then outOfBoundError index
      else do
        arrST <- readSTRef arrR
        writeArray arrST index e

  {-# INLINE mSortOn #-}
  mSortOn :: Ord b => (a -> b) -> MArrayList a s -> ST s ()
  mSortOn f mal@(MArrayList _ arrR) = do
    arrST <- readSTRef arrR
    l     <- size mal
    unsafeQuickSort f 0 l arrST

  mSubList :: Int -> Int -> MArrayList a s -> ST s (MArrayList a s)
  mSubList inf sup mal = do
    ls <- size mal
    let inf' = max inf 0
    let sup' = min sup ls
    let len' = sup' - inf'
    let ps   = initialSize len'
    if sup' <= inf
      then mNewList []
      else do
        resST <- newArray_ (0, ps - 1)
        forM_ [0..(len' - 1)]
          $ \i -> mal `mGet` (i + inf') >>= writeArray resST i
        lR <- newMURef len'
        resR <- newSTRef resST
        return $ MArrayList lR resR

  -- Overwritten default method
  mIndexOf :: Eq a => MArrayList a s -> a -> ST s (Maybe Int)
  mIndexOf mal e = do
     l <- size mal
     mIndexOf' 0 l
    where
      mIndexOf' i l
        | i == l    = return Nothing
        | otherwise = do
          v <- mal `mGet` i
          if v == e
            then return $ Just i
            else mIndexOf' (i + 1) l

  -- Overwritten default method
  mLastIndexOf :: Eq a => MArrayList a s -> a -> ST s (Maybe Int)
  mLastIndexOf mal e = do
     l <- size mal
     mLastIndexOf' (l - 1) l
    where
      mLastIndexOf' (-1) _ = return Nothing
      mLastIndexOf' i l    = do
        v <- mal `mGet` i
        if v == e
          then return $ Just i
          else mLastIndexOf' (i - 1) l


--------------------------------------------------------------------------------
-- MArrayBased Instance
--------------------------------------------------------------------------------

instance MArrayBased MArrayList a ST s where
  mDeepClear :: MArrayList a s -> ST s ()
  mDeepClear (MArrayList lR arrR) = do
    MArrayList rlR resR <- mNewList []
    rl                  <- readMURef rlR
    resST               <- readSTRef resR
    writeMURef lR rl
    writeSTRef arrR resST

  mNewWithSize  :: Foldable f => Int -> f a -> ST s (MArrayList a s)
  mNewWithSize = (arrayListThaw .) . newWithSize

  mPhysicalSize :: MArrayList a s -> ST s Int
  mPhysicalSize (MArrayList _ arrR) = do
    arrST    <- readSTRef arrR
    (_, sup) <- getBounds arrST
    return $ sup + 1

  mResize :: Int -> MArrayList a s -> ST s ()
  mResize s _
    | s < 0 = arrayLengthOverflowError
  mResize s (MArrayList lR arrR) = do
    arrST    <- readSTRef arrR
    l        <- readMURef lR
    (_, sup) <- getBounds arrST
    let s' = max s (sup + 1)
    resST    <- newArray_ (0, s' - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    writeSTRef arrR resST

  trueCopy :: MArrayList a s -> ST s (MArrayList a s)
  trueCopy mal@(MArrayList _ arrR) = do
    ls    <- size mal
    ps    <- mPhysicalSize mal
    arrST <- readSTRef arrR
    resST <- newArray_ (0, ps - 1)
    unsafeCopyArray arrST resST (0, ls - 1)
    rlR   <- newMURef ls
    resR  <- newSTRef resST
    return $ MArrayList rlR resR


--------------------------------------------------------------------------------
-- MQueue Instance
--------------------------------------------------------------------------------

instance MQueue MArrayList a ST s where
  mDequeue :: MArrayList a s -> ST s (Maybe a)
  mDequeue = mPop

  mEnqueue :: a -> MArrayList a s -> ST s ()
  mEnqueue = mPush

  mPeek :: MArrayList a s -> ST s (Maybe a)
  mPeek m = do
    l <- size m
    if l == 0
      then return Nothing
      else Just <$> m `mGet` (l - 1)


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance MDS (MArrayList a) ST s where
  clear :: MArrayList a s -> ST s ()
  clear (MArrayList lR _)
    = writeMURef lR 0

  copy :: MArrayList a s -> ST s (MArrayList a s)
  copy (MArrayList lR arrR) = do
    l     <- readMURef lR
    arrST <- readSTRef arrR
    resST <- newArray_ (0, initialSize l - 1)
    unsafeCopyArray arrST resST (0, l - 1)
    rlR   <- newMURef l
    resR  <- newSTRef resST
    return $ MArrayList rlR resR

  size :: MArrayList a s -> ST s Int
  size (MArrayList lR _)
    = readMURef lR

instance MDSCons [a] (MArrayList a) ST s where
  finish :: MArrayList a s -> ST s [a]
  finish mal = do
    al <- arrayListFreeze mal
    return $ toList al

  new :: [a] -> ST s (MArrayList a s)
  new = arrayListThaw . newList
