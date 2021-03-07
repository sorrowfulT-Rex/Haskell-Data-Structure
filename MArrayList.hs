{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MArrayList where

import           Control.Monad (forM_, liftM2)
import           Control.Monad.ST.Lazy (ST(..), runST)
import           Data.Array.ST
  (STArray(..), freeze, getBounds, newArray_, readArray, thaw, writeArray)
import           Data.Array.Unsafe (unsafeFreeze, unsafeThaw)
import           Data.Foldable (toList)
import           Data.STRef.Lazy (STRef(..), newSTRef, readSTRef, writeSTRef)

import           ArrayBased 
  (ArrayBased(..), MArrayBased(..), arrayLengthOverflowError)
import           ArrayList (ArrayList(..))
import           List 
  (List(..), MList(..), expandedSize, initialSize, outOfBoundError)
import           MDT (MDT(..), MDTCons(..))

data MArrayList e s = MArrayList (STRef s Int) (STRef s (STArray s Int e))


--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

arrayListThaw :: ArrayList a -> ST s (MArrayList a s)
arrayListThaw (ArrayList l arr) = do
  arrST <- thaw arr
  lR    <- newSTRef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR

arrayListFreeze :: MArrayList a s -> ST s (ArrayList a)
arrayListFreeze (MArrayList lR arrR) = do
  l     <- readSTRef lR
  arrST <- readSTRef arrR
  arr   <- freeze arrST
  return $ ArrayList l arr

arrayListThawUnsafe :: ArrayList a -> ST s (MArrayList a s)
arrayListThawUnsafe (ArrayList l arr) = do
  arrST <- unsafeThaw arr
  lR    <- newSTRef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR

arrayListFreezeUnsafe :: MArrayList a s -> ST s (ArrayList a)
arrayListFreezeUnsafe (MArrayList lR arrR) = do
  l     <- readSTRef lR
  arrST <- readSTRef arrR
  arr   <- unsafeFreeze arrST
  return $ ArrayList l arr


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance MList MArrayList where
  mAdd :: Int -> a -> MArrayList a s -> ST s ()
  mAdd index e mal@(MArrayList lR arrR) = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    if index < 0 || index > ls
      then return $ outOfBoundError index
      else if ls == ps
        then do
          resized <- mResize (expandedSize ls) mal
          let MArrayList rlR resR = resized
          rl      <- readSTRef rlR
          writeSTRef lR rl
          resST   <- readSTRef resR
          writeSTRef arrR resST
          mAdd index e resized
        else do
          arrST <- readSTRef arrR
          writeSTRef lR (ls + 1)
          addSTUnsafe index e (ls - 1) arrST

  mClear :: MArrayList a s -> ST s ()
  mClear (MArrayList lR arrR) = writeSTRef lR 0

  mGet :: MArrayList a s -> Int -> ST s a
  mGet mal@(MArrayList lR arrR) index = do
    l <- mSize mal
    if index >= l || index < 0
      then return $ outOfBoundError index
      else readSTRef arrR >>= flip readArray index

  mIndicesOf :: Eq a => MArrayList a s -> a -> ST s [Int]
  mIndicesOf mal e = mSize mal >>= mIndicesOf' 0
    where
      mIndicesOf' i l 
        | i >= l = return []
      mIndicesOf' i l = do
        v <- mal `mGet` i
        if v == e
          then liftM2 (:) (pure i) (mIndicesOf' (i + 1) l)
          else mIndicesOf' (i + 1) l

  mToList :: MArrayList a s -> ST s [a]
  mToList mal = do
    al <- arrayListFreeze mal
    return $ toList al

  mRemove :: Int -> MArrayList a s -> ST s (Maybe a)
  mRemove index mal@(MArrayList lR arrR) = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    if index < 0 || index >= ls
      then return Nothing
      else do
        arrST <- readSTRef arrR
        v     <- readArray arrST index
        writeSTRef lR (ls - 1)
        removeSTUnsafe index (ls - 2) arrST
        return $ Just v

  mSet :: MArrayList a s -> Int -> a -> ST s ()
  mSet mal@(MArrayList _ arrR) index e = do
    ls <- mSize mal
    if index < 0 || index >= ls
      then return $ outOfBoundError index
      else do
        arrST <- readSTRef arrR
        writeArray arrST index e

  mSize :: MArrayList a s -> ST s Int
  mSize (MArrayList lR _)
    = readSTRef lR

  newMList :: Foldable f => f a -> ST s (MArrayList a s)
  newMList = arrayListThaw . newList


--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance MArrayBased MArrayList where
  mDeepClear :: MArrayList a s -> ST s ()
  mDeepClear (MArrayList lR arrR) = do
    MArrayList rlR resR <- newMList []
    rl                  <- readSTRef rlR
    resST               <- readSTRef resR
    writeSTRef lR rl
    writeSTRef arrR resST

  mNewWithSize  :: Foldable f => Int -> f a -> ST s (MArrayList a s)
  mNewWithSize = (arrayListThaw .) . newWithSize

  mPhysicalSize :: MArrayList a s -> ST s Int
  mPhysicalSize (MArrayList _ arrR) = do
    arrST    <- readSTRef arrR
    (_, sup) <- getBounds arrST
    return $ sup + 1

  mResize :: Int -> MArrayList a s -> ST s (MArrayList a s)
  mResize s _
    | s < 0 = return arrayLengthOverflowError
  mResize s (MArrayList lR arrR) = do
    arrST    <- readSTRef arrR
    l        <- readSTRef lR
    (_, sup) <- getBounds arrST
    let s' = max s (sup + 1)
    resST    <- newArray_ (0, s' - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    resR     <- newSTRef resST
    return $ MArrayList lR resR

  trueCopy :: MArrayList a s -> ST s (MArrayList a s)
  trueCopy mal@(MArrayList _ arrR) = do
    ls    <- mSize mal
    ps    <- mPhysicalSize mal
    arrST <- readSTRef arrR
    resST <- newArray_ (0, ps - 1)
    copyArrayUnsafe arrST resST (0, ls - 1)
    rlR   <- newSTRef ls
    resR  <- newSTRef resST
    return $ MArrayList rlR resR


--------------------------------------------------------------------------------
-- MDT Functions
--------------------------------------------------------------------------------

instance MDT (MArrayList a) s where
  copy :: MArrayList a s -> ST s (MArrayList a s)
  copy (MArrayList lR arrR) = do
    l     <- readSTRef lR
    arrST <- readSTRef arrR
    resST <- newArray_ (0, initialSize l - 1)
    copyArrayUnsafe arrST resST (0, l - 1)
    rlR   <- newSTRef l
    resR  <- newSTRef resST
    return $ MArrayList rlR resR

instance Foldable f => MDTCons (f a) (MArrayList a) s where
  new :: f a -> ST s (MArrayList a s)
  new = newMList


--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

addSTUnsafe :: Int -> a -> Int -> STArray s Int a -> ST s ()
addSTUnsafe index e lastElement arrST = do
  forM_ [lastElement, (lastElement - 1)..index] $ \i -> do
    v <- readArray arrST i
    writeArray arrST (i + 1) v
  writeArray arrST index e

copyArrayUnsafe :: STArray s Int a -> STArray s Int a -> (Int, Int) -> ST s ()
copyArrayUnsafe arrST resST (inf, sup) = do
  forM_ (zip [0..] [inf..sup]) $ 
    \(i, i') -> readArray arrST i >>= writeArray resST i'

removeSTUnsafe :: Int -> Int -> STArray s Int a -> ST s ()
removeSTUnsafe index lastElement arrST
  = forM_ [index..lastElement] $ \i -> do
    v <- readArray arrST (i + 1)
    writeArray arrST i v


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

data D = forall a. Show a => D a

instance Show D where
  show (D a) = show a

foom :: IO ()
foom = do
  print $ runST $ do
    mal  <- new (replicate 1000000 (1::Int)) :: ST s (MArrayList Int s)
    il   <- mal `mIndicesOf` 1
    return [D $ (head il)]
