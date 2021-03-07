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

-- | @MArrayList@ is a data structure implementing the 'MList' class with an
-- internal @STArray@.
data MArrayList e s = MArrayList (STRef s Int) (STRef s (STArray s Int e))


--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

-- | Makes a mutable @MArrayList@ from an immutable @ArrayList@ by copying. 
arrayListThaw :: ArrayList a -> ST s (MArrayList a s)
arrayListThaw (ArrayList l arr) = do
  arrST <- thaw arr
  lR    <- newSTRef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR

-- | Makes a immutable @ArrayList@ from a mutable @MArrayList@ by copying. 
arrayListFreeze :: MArrayList a s -> ST s (ArrayList a)
arrayListFreeze (MArrayList lR arrR) = do
  l     <- readSTRef lR
  arrST <- readSTRef arrR
  arr   <- freeze arrST
  return $ ArrayList l arr

-- | Unsafe Function.
-- Makes a mutable @MArrayList@ from an immutable @ArrayList@, perhaps without
-- copying.
-- The original immutable list should not be used ever since.
arrayListThawUnsafe :: ArrayList a -> ST s (MArrayList a s)
arrayListThawUnsafe (ArrayList l arr) = do
  arrST <- unsafeThaw arr
  lR    <- newSTRef l
  arrR  <- newSTRef arrST
  return $ MArrayList lR arrR

-- | Unsafe Function.
-- Makes an immutable @ArrayList@ from a mutable @MArrayList@, perhaps without
-- copying.
-- The original mutable list should not be used ever since.
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

  mDelete :: Int -> MArrayList a s -> ST s (Maybe a)
  mDelete index mal@(MArrayList lR arrR) = do
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

  mSubList :: Int -> Int -> MArrayList a s -> ST s (MArrayList a s)
  mSubList inf sup mal = do
    ls <- mSize mal
    let inf' = max inf 0
    let sup' = min sup ls
    let len' = sup' - inf'
    let ps   = initialSize len'
    if sup' <= inf
      then newMList []
      else do
        resST <- newArray_ (0, ps - 1)
        forM_ [0..(len' - 1)] 
          $ \i -> mal `mGet` (i + inf') >>= writeArray resST i
        lR <- newSTRef len'
        resR <- newSTRef resST
        return $ MArrayList lR resR

  mToList :: MArrayList a s -> ST s [a]
  mToList mal = do
    al <- arrayListFreeze mal
    return $ toList al

  newMList :: Foldable f => f a -> ST s (MArrayList a s)
  newMList = arrayListThaw . newList

  -- Overwritten default methods
  mLastIndexOf :: Eq a => MArrayList a s -> a -> ST s (Maybe Int)
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

  newMWithSize  :: Foldable f => Int -> f a -> ST s (MArrayList a s)
  newMWithSize = (arrayListThaw .) . newWithSize

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

-- | Unsafe: Does not check conduct bound check for array.
-- Takes an @Int@ as the starting index, an element, an @Int@ as the last index
-- to be written to, and an @Int@-indexed @STArray@, pushes all elements since
-- the starting index (inclusive) to the last index (exclusive) to the next 
-- slot, opening a vacancy at the starting index, where it puts the given
--  element to this index.
-- Pre: The index bounds are valid.
addSTUnsafe :: Int -> a -> Int -> STArray s Int a -> ST s ()
addSTUnsafe index e lastIndexOf arrST = do
  forM_ [lastIndexOf, (lastIndexOf - 1)..index] $ \i -> do
    v <- readArray arrST i
    writeArray arrST (i + 1) v
  writeArray arrST index e

-- | Unsafe: Does not check conduct bound check for array.
-- Takes an @Int@-indexed @STArray@ as the starting array, another
-- @Int@-indexed @STArray@ as the destination, and a tuple of @Int@s as the
-- lower and upper bound (both inclusive) of the range of indices.
-- Copies the elements in the starting array within the range to the
-- destination whose index starts from 0.
-- Pre: The index bounds are valid and the destination array is large enough
-- to hold the number of elements.
copyArrayUnsafe :: STArray s Int a -> STArray s Int a -> (Int, Int) -> ST s ()
copyArrayUnsafe arrST resST (inf, sup) = do
  forM_ (zip [0..] [inf..sup]) $ 
    \(i, i') -> readArray arrST i >>= writeArray resST i'

-- | Unsafe: Does not check conduct bound check for array.
-- TODO...
-- Takes a ordering function, a index lower bound (inclusive), an index upper
-- bound (exclusive) and an @Int@-indexed @STArray@, sorts the array.
-- Pre: The index bounds are valid and the array must be @Int@-indexed from 0.
heapSortUnsafe :: Ord b => (a -> b) -> Int -> Int -> STArray s Int a -> ST s ()
heapSortUnsafe = undefined 

-- | Unsafe: Does not check conduct bound check for array.
-- Takes an @Int@ as the starting index, an element, an @Int@ as the last index
-- to be written to, and an @Int@-indexed @STArray@, pushes all elements since
-- the starting index (exclusive) to the last index (inclusive) to the previous 
-- slot, effectively removing the original element at the starting index.
-- Pre: The index bounds are valid and the last index is less than the physical
-- length of the array minus 1.
removeSTUnsafe :: Int -> Int -> STArray s Int a -> ST s ()
removeSTUnsafe index lastIndexOf arrST
  = forM_ [index..lastIndexOf] $ \i -> do
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
    mal <- new [1..10] :: ST s (MArrayList Integer s)
    v1  <- mRemove 7 mal
    v2  <- mRemove 13 mal
    al  <- arrayListFreeze mal
    return [D v1, D v2, D al]
