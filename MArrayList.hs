{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module MArrayList where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
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


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance MList MArrayList where
  mAdd :: Int -> e -> MArrayList s e -> ST s ()
  mAdd index e mal = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    let MArrayList lR arrR = mal
    if index < 0 || index > ls
      then return $ outOfBoundError index
      else if ls == ps
        then do
          resized <- mResize ((3 * ls) `div` 2) mal
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

  mRemove :: Int -> MArrayList s e -> ST s (Maybe e)
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

  mSize :: MArrayList s e -> ST s Int
  mSize (MArrayList lR _)
    = readSTRef lR

  newMList :: Foldable f => f e -> ST s (MArrayList s e)
  newMList = arrayListThaw . newList


--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance MArrayBased MArrayList where
  mNewWithSize  :: Foldable f => Int -> f e -> ST s (MArrayList s e)
  mNewWithSize = (arrayListThaw .) . newWithSize

  mPhysicalSize :: MArrayList s e -> ST s Int
  mPhysicalSize (MArrayList _ arrR) = do
    arrST    <- readSTRef arrR
    (_, sup) <- getBounds arrST
    return $ sup + 1

  mResize :: Int -> MArrayList s e -> ST s (MArrayList s e)
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
  let al = runST $ do
      mal <- newMList [10, 20, 30]
      p1  <- mPhysicalSize mal
      mAppend 50 mal
      p2  <- mPhysicalSize mal
      mAdd 3 40 mal
      p3  <- mPhysicalSize mal
      v1  <- mPop mal
      v2  <- mPopEnd mal
      v3  <- mRemove 1 mal
      al  <- arrayListFreeze mal
      return [D p1, D p2, D p3, D v1, D v2, D v3, D al]
  print al
