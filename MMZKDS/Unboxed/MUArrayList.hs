{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.MUArrayList where

import           Control.Monad (forM_, liftM2)
import           Control.Monad.ST (ST(..), runST)
import           Data.Array.ST
  (STUArray(..), MArray(..), freeze, getBounds, newArray_, readArray, thaw, 
   writeArray
  )
import           Data.Array.Unboxed (IArray(..), UArray(..))
import           Data.Array.Unsafe (unsafeFreeze, unsafeThaw)
import           Data.STRef (STRef(..), newSTRef, readSTRef, writeSTRef)

import           MMZKDS.ArrayBased (ArrayBased(..), MArrayBased(..))
import           MMZKDS.Unboxed.MURef 
  (MURef(..), newMURef, readMURef, writeMURef)
import           MMZKDS.Unboxed.UArrayList (UArrayList(..))
import           MMZKDS.List (List(newList, toList), MList(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Unsafe 
  (unsafeAddST, unsafeCopyArray, unsafeQuickSort, unsafeRemoveST)
import           MMZKDS.Utilities
  (arrayLengthOverflowError, expandedSize, initialSize, outOfBoundError)

-- | @MUArrayList@ is a data structure implementing the 'MList' class with an
-- internal @STUArray@.
--
data MUArrayList e s = MUArrayList (MURef s Int) (STRef s (STUArray s Int e))


--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

-- | Makes a mutable @MUArrayList@ from an immutable @ArrayList@ by copying. 
--
uArrayListThaw :: forall a s. (IArray UArray a, MArray (STUArray s) a (ST s)) 
               => UArrayList a 
               -> ST s (MUArrayList a s)
uArrayListThaw (UArrayList l arr) = do
  arrST <- thaw arr :: ST s (STUArray s Int a)
  lR    <- newMURef l
  arrR  <- newSTRef arrST
  return $ MUArrayList lR arrR

-- | Makes a immutable @UArrayList@ from a mutable @MUArrayList@ by copying. 
--
uArrayListFreeze :: forall a s. (IArray UArray a, MArray (STUArray s) a (ST s)) 
                 => MUArrayList a s 
                 -> ST s (UArrayList a)
uArrayListFreeze (MUArrayList lR arrR) = do
  l     <- readMURef lR
  arrST <- readSTRef arrR
  arr   <- freeze arrST
  return $ UArrayList l arr

-- | Unsafe Function.
-- Makes a mutable @MUArrayList@ from an immutable @UArrayList@, perhaps without
-- copying.
-- The original immutable list should not be used ever since.
--
unsafeUArrayListThaw 
  :: forall a s. (IArray UArray a, MArray (STUArray s) a (ST s))
  => UArrayList a 
  -> ST s (MUArrayList a s)
unsafeUArrayListThaw (UArrayList l arr) = do
  arrST <- unsafeThaw arr
  lR    <- newMURef l
  arrR  <- newSTRef arrST
  return $ MUArrayList lR arrR

-- | Unsafe Function.
-- Makes an immutable @UArrayList@ from a mutable @MUArrayList@, perhaps without
-- copying.
-- The original mutable list should not be used ever since.
--
unsafeUArrayListFreeze 
  :: forall a s. (IArray UArray a, MArray (STUArray s) a (ST s))
  => MUArrayList a s 
  -> ST s (UArrayList a)
unsafeUArrayListFreeze (MUArrayList lR arrR) = do
  l     <- readMURef lR
  arrST <- readSTRef arrR
  arr   <- unsafeFreeze arrST
  return $ UArrayList l arr


--------------------------------------------------------------------------------
-- MList Instance
--------------------------------------------------------------------------------

instance (IArray UArray a, MArray (STUArray s) a (ST s)) 
  => MList MUArrayList a ST s where
  mClear :: MUArrayList a s -> ST s ()
  mClear (MUArrayList lR arrR) 
    = writeMURef lR 0

  mGet :: MUArrayList a s -> Int -> ST s a
  mGet mal@(MUArrayList lR arrR) index = do
    l <- mSize mal
    if index >= l || index < 0
      then return $ outOfBoundError index
      else readSTRef arrR >>= flip readArray index

  mIndicesOf :: Eq a => MUArrayList a s -> a -> ST s [Int]
  mIndicesOf mal e = mSize mal >>= mIndicesOf' 0
    where
      mIndicesOf' i l 
        | i >= l = return []
      mIndicesOf' i l = do
        v <- mal `mGet` i
        if v == e
          then liftM2 (:) (pure i) (mIndicesOf' (i + 1) l)
          else mIndicesOf' (i + 1) l

  mDelete :: Int -> MUArrayList a s -> ST s (Maybe a)
  mDelete index mal@(MUArrayList lR arrR) = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    if index < 0 || index >= ls
      then return Nothing
      else do
        arrST <- readSTRef arrR
        v     <- readArray arrST index
        writeMURef lR (ls - 1)
        unsafeRemoveST index (ls - 2) arrST
        return $ Just v

  mInsert :: Int -> a -> MUArrayList a s -> ST s ()
  mInsert index e mal@(MUArrayList lR arrR) = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    if index < 0 || index > ls
      then return $ outOfBoundError index
      else if ls == ps
        then do
          resized <- mResize (expandedSize ls) mal
          let MUArrayList rlR resR = resized
          rl      <- readMURef rlR
          writeMURef lR rl
          resST   <- readSTRef resR
          writeSTRef arrR resST
          mInsert index e resized
        else do
          arrST <- readSTRef arrR
          writeMURef lR (ls + 1)
          unsafeAddST index e (ls - 1) arrST

  mSet :: MUArrayList a s -> Int -> a -> ST s ()
  mSet mal@(MUArrayList _ arrR) index e = do
    ls <- mSize mal
    if index < 0 || index >= ls
      then return $ outOfBoundError index
      else do
        arrST <- readSTRef arrR
        writeArray arrST index e

  mSize :: MUArrayList a s -> ST s Int
  mSize (MUArrayList lR _)
    = readMURef lR

  {-# INLINE mSortOn #-}
  mSortOn :: Ord b => (a -> b) -> MUArrayList a s -> ST s ()
  mSortOn f mal@(MUArrayList _ arrR) = do
    arrST <- readSTRef arrR 
    l     <- mSize mal
    unsafeQuickSort f 0 l arrST

  mSubList :: Int -> Int -> MUArrayList a s -> ST s (MUArrayList a s)
  mSubList inf sup mal = do
    ls <- mSize mal
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
        return $ MUArrayList lR resR

  mToList :: MUArrayList a s -> ST s [a]
  mToList mal = do
    al <- uArrayListFreeze mal
    return $ toList al

  mNewList :: Foldable f => f a -> ST s (MUArrayList a s)
  mNewList = uArrayListThaw . newList

  -- Overwritten default method
  mIndexOf :: Eq a => MUArrayList a s -> a -> ST s (Maybe Int)
  mIndexOf mal e = do
     l <- mSize mal
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
  mLastIndexOf :: Eq a => MUArrayList a s -> a -> ST s (Maybe Int)
  mLastIndexOf mal e = do
     l <- mSize mal
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

instance (IArray UArray a, MArray (STUArray s) a (ST s)) 
  => MArrayBased MUArrayList a ST s where
  mDeepClear :: MUArrayList a s -> ST s ()
  mDeepClear (MUArrayList lR arrR) = do
    MUArrayList rlR resR <- mNewList []
    rl                   <- readMURef rlR
    resST                <- readSTRef resR
    writeMURef lR rl
    writeSTRef arrR resST

  mNewWithSize  :: Foldable f => Int -> f a -> ST s (MUArrayList a s)
  mNewWithSize = (uArrayListThaw .) . newWithSize

  mPhysicalSize :: MUArrayList a s -> ST s Int
  mPhysicalSize (MUArrayList _ arrR) = do
    arrST    <- readSTRef arrR
    (_, sup) <- getBounds arrST
    return $ sup + 1

  mResize :: Int -> MUArrayList a s -> ST s (MUArrayList a s)
  mResize s _
    | s < 0 = return arrayLengthOverflowError
  mResize s (MUArrayList lR arrR) = do
    arrST    <- readSTRef arrR
    l        <- readMURef lR
    (_, sup) <- getBounds arrST
    let s' = max s (sup + 1)
    resST    <- newArray_ (0, s' - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    resR     <- newSTRef resST
    return $ MUArrayList lR resR

  trueCopy :: MUArrayList a s -> ST s (MUArrayList a s)
  trueCopy mal@(MUArrayList _ arrR) = do
    ls    <- mSize mal
    ps    <- mPhysicalSize mal
    arrST <- readSTRef arrR
    resST <- newArray_ (0, ps - 1)
    unsafeCopyArray arrST resST (0, ls - 1)
    rlR   <- newMURef ls
    resR  <- newSTRef resST
    return $ MUArrayList rlR resR


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance (IArray UArray a, MArray (STUArray s) a (ST s)) =>
  MDS (MUArrayList a) s where
  copy :: MUArrayList a s -> ST s (MUArrayList a s)
  copy (MUArrayList lR arrR) = do
    l     <- readMURef lR
    arrST <- readSTRef arrR
    resST <- newArray_ (0, initialSize l - 1)
    unsafeCopyArray arrST resST (0, l - 1)
    rlR   <- newMURef l
    resR  <- newSTRef resST
    return $ MUArrayList rlR resR

instance (IArray UArray a, MArray (STUArray s) a (ST s)) 
  => MDSCons [a] (MUArrayList a) s where
  finish :: MUArrayList a s -> ST s [a]
  finish = mToList

  new :: [a] -> ST s (MUArrayList a s)
  new = mNewList


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

data D = forall a. Show a => D a

instance Show D where
  show (D a) = show a

foom :: IO ()
foom = do
  print $ runST $ do
    mal <- mNewList [100,99..1] :: ST s (MUArrayList Int s)
    mSort mal
    al  <- uArrayListFreeze mal
    return [D al, D $ physicalSize al]
