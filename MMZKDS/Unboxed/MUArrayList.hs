{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Unboxed.MUArrayList (MUArrayList) where

import           Control.Monad (forM_, liftM2, when)
import           Control.Monad.ST (ST)
import           Data.Array.ST
  (STUArray, MArray, getBounds, newArray_, readArray, writeArray)
import           Data.Array.Unboxed (IArray, UArray)
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import           Unsafe.Coerce (unsafeCoerce)

import qualified MMZKDS.Class.ArrayBased as AB (ArrayBased(..)) 
import qualified MMZKDS.Class.List as L (List(newList, toList))
import           MMZKDS.Class.MArrayBased (MArrayBased(..))
import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))
import           MMZKDS.Class.MList (MList(..))
import           MMZKDS.Class.MQueue (MDeque(..))
import           MMZKDS.Unboxed.Base
  (MUArrayList(..), uArrayListFreeze, uArrayListThaw)
import           MMZKDS.Unboxed.STURef
  (STU, STURef, newSTURef, readSTURef, writeSTURef)
import           MMZKDS.Unboxed.UArrayList ()
import           MMZKDS.Unsafe
  ( unsafeAddST, unsafeAddAllST, unsafeCopyArray, unsafeQuickSort
  , unsafeRemoveST )
import           MMZKDS.Utilities
  ( arrayLengthOverflowError, expandedSize, idMUArrayList, initialSize
  , outOfBoundError
  )


--------------------------------------------------------------------------------
-- MList Instance
--------------------------------------------------------------------------------

instance (IArray UArray a, STU a s) => MList (MUArrayList a) a ST s where
  get :: MUArrayList a s -> Int -> ST s a
  get mal@(MUArrayList lR arrR) index = do
    l <- size mal
    if index >= l || index < 0
      then return $ outOfBoundError index
      else readSTRef arrR >>= flip readArray index

  indicesOf :: Eq a => MUArrayList a s -> a -> ST s [Int]
  indicesOf mal e = size mal >>= mIndicesOf' 0
    where
      mIndicesOf' i l
        | i >= l = return []
      mIndicesOf' i l = do
        v <- mal `get` i
        if v == e
          then fmap (i :) (mIndicesOf' (i + 1) l)
          else mIndicesOf' (i + 1) l

  delete :: MUArrayList a s -> Int -> ST s (Maybe a)
  delete mal@(MUArrayList lR arrR) index = do
    ls <- size mal
    ps <- physicalSize mal
    if index < 0 || index >= ls
      then return Nothing
      else do
        arrST <- readSTRef arrR
        v     <- readArray arrST index
        writeSTURef lR (ls - 1)
        unsafeRemoveST index index (ls - 2) arrST
        return $ Just v

  insert :: MUArrayList a s -> Int -> a -> ST s ()
  insert mal@(MUArrayList lR arrR) index e = do
    ls <- size mal
    ps <- physicalSize mal
    if      index < 0 || index > ls
    then    return $ outOfBoundError index
    else if ls == ps
    then do
      resize (expandedSize ls) mal
      insert mal index e
    else do
      arrST <- readSTRef arrR
      writeSTURef lR (ls + 1)
      unsafeAddST index e (ls - 1) arrST

  set :: MUArrayList a s -> Int -> a -> ST s ()
  set mal@(MUArrayList _ arrR) index e = do
    ls <- size mal
    if index < 0 || index >= ls
      then return $ outOfBoundError index
      else do
        arrST <- readSTRef arrR
        writeArray arrST index e

  {-# INLINE sortOn #-}
  sortOn :: Ord b => (a -> b) -> MUArrayList a s -> ST s ()
  sortOn f mal@(MUArrayList _ arrR) = do
    arrST <- readSTRef arrR
    l     <- size mal
    unsafeQuickSort f 0 l arrST

  subList :: MUArrayList a s -> Int -> Int -> ST s (MUArrayList a s)
  subList mal inf sup = do
    ls <- size mal
    let inf' = max inf 0
    let sup' = min sup ls
    let len' = sup' - inf'
    let ps   = initialSize len'
    if sup' <= inf
      then newList []
      else do
        resST <- newArray_ (0, ps - 1)
        let MUArrayList _ arrR = mal
        arrST <- readSTRef arrR
        unsafeCopyArray arrST resST (inf', sup')
        lR    <- newSTURef len'
        resR  <- newSTRef resST
        return $ MUArrayList lR resR

  -- Overwritten default method
  deleteRange :: MUArrayList a s -> Int -> Int -> ST s [a]
  deleteRange mal@(MUArrayList lR arrR) inf sup = do
    ls    <- size mal
    let inf' = max inf 0
    let sup' = min sup ls
    let diff = sup' - inf'
    arrST <- readSTRef arrR
    es    <- subList mal inf sup >>= toList
    writeSTURef lR $! ls - diff
    unsafeRemoveST inf' (sup' - 1) (ls - diff - 1) arrST
    return es

  -- Overwritten default method
  indexOf :: Eq a => MUArrayList a s -> a -> ST s (Maybe Int)
  indexOf mal e = do
     l <- size mal
     mIndexOf' 0 l
    where
      mIndexOf' i l
        | i == l    = return Nothing
        | otherwise = do
          v <- mal `get` i
          if v == e
            then return $ Just i
            else mIndexOf' (i + 1) l

  -- Overwritten default method
  insertAll :: forall l. (MDSCons [a] l ST s, MDS l ST s)
            => MUArrayList a s
            -> Int
            -> l s
            -> ST s ()
  insertAll mal@(MUArrayList lR arrR) index es = do
    name <- identifier mal
    if name == idMUArrayList
      then insertAll' mal index (unsafeCoerce es)
      else do
        ls  <- size mal
        ls' <- size es
        ps  <- physicalSize mal
        xs  <- finish es
        let ls'' = ls + ls'
        let end  = index + ls' - 1
        if      index < 0 || index > ls
        then    outOfBoundError index
        else if ls'' >= ps
        then    resize (expandedSize ls'') mal >> insertAll mal index es
        else do
          writeSTURef lR ls''
          arrST <- readSTRef arrR
          unsafeAddAllST index ls' xs (ls - 1) arrST

  -- Overwritten default method
  insertAll' :: MUArrayList a s -> Int -> MUArrayList a s -> ST s ()
  insertAll' mal@(MUArrayList lR arrR) index mal' = do
    ls  <- size mal
    ls' <- size mal'
    ps  <- physicalSize mal
    let ls'' = ls + ls'
    let end  = index + ls' - 1
    if      index < 0 || index > ls
    then    outOfBoundError index
    else if ls'' >= ps
    then    resize (expandedSize ls'') mal >> insertAll' mal index mal'
    else do
      writeSTURef lR ls''
      forM_ [ls - 1, ls - 2..index] $ \i -> mal `get` i >>= set mal (i + ls')
      forM_ [index..end] $ \i -> mal' `get` (i - index) >>= set mal i

  -- Overwritten default method
  lastIndexOf :: Eq a => MUArrayList a s -> a -> ST s (Maybe Int)
  lastIndexOf mal e = do
     l <- size mal
     mLastIndexOf' (l - 1) l
    where
      mLastIndexOf' (-1) _ = return Nothing
      mLastIndexOf' i l    = do
        v <- mal `get` i
        if v == e
          then return $ Just i
          else mLastIndexOf' (i - 1) l


--------------------------------------------------------------------------------
-- MArrayBased Instance
--------------------------------------------------------------------------------

instance (IArray UArray a, STU a s) => MArrayBased (MUArrayList a) a ST s where
  deepClear :: MUArrayList a s -> ST s ()
  deepClear (MUArrayList lR arrR) = do
    MUArrayList rlR resR <- newList []
    rl                   <- readSTURef rlR
    resST                <- readSTRef resR
    writeSTURef lR rl
    writeSTRef arrR resST

  newWithSize  :: Foldable f => Int -> f a -> ST s (MUArrayList a s)
  newWithSize = (uArrayListThaw .) . AB.newWithSize

  physicalSize :: MUArrayList a s -> ST s Int
  physicalSize (MUArrayList _ arrR) = do
    arrST    <- readSTRef arrR
    (_, sup) <- getBounds arrST
    return $ sup + 1

  resize :: Int -> MUArrayList a s -> ST s ()
  resize s _
    | s < 0 = return arrayLengthOverflowError
  resize s (MUArrayList lR arrR) = do
    arrST    <- readSTRef arrR
    l        <- readSTURef lR
    (_, sup) <- getBounds arrST
    let s' = max s (sup + 1)
    resST    <- newArray_ (0, s' - 1)
    forM_ [0..(l - 1)] $ \i -> do
      v <- readArray arrST i
      writeArray resST i v
    writeSTRef arrR resST

  trueCopy :: MUArrayList a s -> ST s (MUArrayList a s)
  trueCopy mal@(MUArrayList _ arrR) = do
    ls    <- size mal
    ps    <- physicalSize mal
    arrST <- readSTRef arrR
    resST <- newArray_ (0, ps - 1)
    unsafeCopyArray arrST resST (0, ls - 1)
    rlR   <- newSTURef ls
    resR  <- newSTRef resST
    return $ MUArrayList rlR resR


--------------------------------------------------------------------------------
-- MDeque Instance
--------------------------------------------------------------------------------

instance (IArray UArray a, STU a s) => MDeque (MUArrayList a) a ST s where
  dequeueFront :: MUArrayList a s -> ST s (Maybe a)
  dequeueFront = popFront

  dequeueEnd :: MUArrayList a s -> ST s (Maybe a)
  dequeueEnd = pop

  enqueueFront :: MUArrayList a s -> a -> ST s ()
  enqueueFront = push

  enqueueEnd :: MUArrayList a s -> a -> ST s ()
  enqueueEnd = append


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance (IArray UArray a, STU a s) => MDS (MUArrayList a) ST s where
  clear :: MUArrayList a s -> ST s ()
  clear (MUArrayList lR _)
    = writeSTURef lR 0

  copy :: MUArrayList a s -> ST s (MUArrayList a s)
  copy (MUArrayList lR arrR) = do
    l     <- readSTURef lR
    arrST <- readSTRef arrR
    resST <- newArray_ (0, initialSize l - 1)
    unsafeCopyArray arrST resST (0, l - 1)
    rlR   <- newSTURef l
    resR  <- newSTRef resST
    return $ MUArrayList rlR resR

  identifier :: MUArrayList a s -> ST s String
  identifier = const $ return idMUArrayList

  size :: MUArrayList a s -> ST s Int
  size (MUArrayList lR _)
    = readSTURef lR

instance (IArray UArray a, MArray (STUArray s) a (ST s))
  => MDSCons [a] (MUArrayList a) ST s where
  finish :: MUArrayList a s -> ST s [a]
  finish mal = do
    al <- uArrayListFreeze mal
    return $ L.toList al

  new :: [a] -> ST s (MUArrayList a s)
  new = uArrayListThaw . L.newList
