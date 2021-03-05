{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module MArrayList where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe
import           Data.Bits
import           Data.Foldable
import           Data.STRef

import           ArrayBased
import           ArrayList
import           List

data MArrayList s e = MArrayList (STRef s Int) (STRef s (STArray s Int e))


--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

arrayListThaw :: ArrayList a -> ST s (MArrayList s a)
arrayListThaw (ArrayList l arr) = do
  arrST <- thaw arr
  lR    <- newSTRef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR

arrayListFreeze :: MArrayList s a -> ST s (ArrayList a)
arrayListFreeze (MArrayList lR arrR) = do
  l     <- readSTRef lR
  arrST <- readSTRef arrR
  arr   <- freeze arrST
  return $ ArrayList l arr

arrayListThawUnsafe :: ArrayList a -> ST s (MArrayList s a)
arrayListThawUnsafe (ArrayList l arr) = do
  arrST <- unsafeThaw arr
  lR    <- newSTRef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR

arrayListFreezeUnsafe :: MArrayList s a -> ST s (ArrayList a)
arrayListFreezeUnsafe (MArrayList lR arrR) = do
  l     <- readSTRef lR
  arrST <- readSTRef arrR
  arr   <- unsafeFreeze arrST
  return $ ArrayList l arr


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance MList MArrayList where
  mAdd :: Int -> a -> MArrayList s a -> ST s ()
  mAdd index e mal = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    let MArrayList lR arrR = mal
    if index < 0 || index > ls
      then return $ outOfBoundError index
      else if ls == ps
        then do
          resized <- mResize (1 + (3 * ls) `div` 2) mal
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

  mClear :: MArrayList s a -> ST s ()
  mClear (MArrayList lR arrR) = writeSTRef lR 0

  mToList :: MArrayList s a -> ST s [a]
  mToList mal = do
    al <- arrayListFreeze mal
    return $ toList al

  mRemove :: Int -> MArrayList s a -> ST s (Maybe a)
  mRemove index mal = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    if index < 0 || index >= ls
      then return Nothing
      else do
        let MArrayList lR arrR = mal
        arrST <- readSTRef arrR
        v     <- readArray arrST index
        writeSTRef lR (ls - 1)
        removeSTUnsafe index (ls - 2) arrST
        return $ Just v

  mSize :: MArrayList s a -> ST s Int
  mSize (MArrayList lR _)
    = readSTRef lR

  newMList :: Foldable f => f a -> ST s (MArrayList s a)
  newMList = arrayListThaw . newList


--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance MArrayBased MArrayList where
  mDeepClear :: MArrayList s a -> ST s ()
  mDeepClear (MArrayList lR arrR) = do
    MArrayList rlR resR <- newMList []
    rl                  <- readSTRef rlR
    resST               <- readSTRef resR
    writeSTRef lR rl
    writeSTRef arrR resST

  mNewWithSize  :: Foldable f => Int -> f a -> ST s (MArrayList s a)
  mNewWithSize = (arrayListThaw .) . newWithSize

  mPhysicalSize :: MArrayList s a -> ST s Int
  mPhysicalSize (MArrayList _ arrR) = do
    arrST    <- readSTRef arrR
    (_, sup) <- getBounds arrST
    return $ sup + 1

  mResize :: Int -> MArrayList s a -> ST s (MArrayList s a)
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


--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

addSTUnsafe :: Int -> a -> Int -> STArray s Int a -> ST s ()
addSTUnsafe index e lastElement arrST = do
  forM_ [lastElement, (lastElement - 1)..index] $ \i -> do
    v <- readArray arrST i
    writeArray arrST (i + 1) v
  writeArray arrST index e

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
  let output = runST $ do
      mal <- newMList [10, 20, 30] :: ST s (MArrayList s Int)
      p1  <- mPhysicalSize mal
      mAppend 50 mal
      p2  <- mPhysicalSize mal
      mAdd 3 40 mal
      p3  <- mPhysicalSize mal
      v1  <- mPop mal
      v2  <- mPopEnd mal
      v3  <- mRemove 1 mal
      mDeepClear mal
      p4  <- mPhysicalSize mal
      al  <- mToList mal
      return [D p1, D p2, D p3, D v1, D v2, D v3, D p4, D al]
  print output
