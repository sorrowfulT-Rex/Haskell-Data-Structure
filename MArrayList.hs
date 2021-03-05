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

import           ArrayBased
import           ArrayList
import           List

data MArrayList s e = MArrayList !Int (STArray s Int e)


--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

arrayListThaw :: ArrayList a -> ST s (MArrayList s a)
arrayListThaw (ArrayList l arr) = do
  arrST <- thaw arr
  return $ MArrayList l arrST

arrayListFreeze :: MArrayList s a -> ST s (ArrayList a)
arrayListFreeze (MArrayList l arrST) = do
  arr <- freeze arrST
  return $ ArrayList l arr


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance MList MArrayList where
  mAdd :: Int -> e -> MArrayList s e -> ST s (MArrayList s e)
  mAdd index e mal = do
    ls <- mSize mal
    ps <- mPhysicalSize mal
    if index < 0 || index > ls
      then return (error $ "Index " ++ show index ++ " is out of bound!")
      else if ls == ps
        then do
          resized <- mResize ((3 * ls) `div` 2) mal
          mAdd index e resized
        else do
          let MArrayList _ arrST = mal
          addST index e (ls - 1) arrST
          return $ MArrayList (ls + 1) arrST

  mSize :: MArrayList s e -> ST s Int
  mSize (MArrayList l _)
    = return l

  newMList :: Foldable f => f e -> ST s (MArrayList s e)
  newMList = arrayListThaw . newList


--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance MArrayBased MArrayList where
  mNewWithSize  :: Foldable f => Int -> f e -> ST s (MArrayList s e)
  mNewWithSize = (arrayListThaw .) . newWithSize

  mPhysicalSize :: MArrayList s e -> ST s Int
  mPhysicalSize (MArrayList l arrST) = do
    (_, sup) <- getBounds arrST
    return $ sup + 1

  mResize :: Int -> MArrayList s e -> ST s (MArrayList s e)
  mResize s (MArrayList l arrST) = do
    (_, sup) <- getBounds arrST
    let s' = max s (sup + 1)
    resST <- newArray_ (0, s' - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    return $ MArrayList l resST


--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Pre: The array has at least one vacent space
addST :: Int -> a -> Int -> STArray s Int a -> ST s ()
addST index e lastElement arrST = do
  forM_ [lastElement, (lastElement - 1)..index] $ \i -> do
    v <- readArray arrST i
    writeArray arrST (i + 1) v
  writeArray arrST index e


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
      mal <- mAppend 10 mal
      p2  <- mPhysicalSize mal
      mal <- mAppend 13 mal
      p3  <- mPhysicalSize mal
      mal <- mAdd 2 114514 mal
      al  <- arrayListFreeze mal
      return [D p1, D p2, D p3, D al]
  print al
