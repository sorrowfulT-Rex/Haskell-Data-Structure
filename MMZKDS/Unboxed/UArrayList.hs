{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Unboxed.UArrayList (UArrayList) where

import           Control.Monad (join)
import           Data.Array.Unboxed (IArray, UArray, accum, array, bounds, (!))
import           Data.Foldable as F (toList)
import           Unsafe.Coerce (unsafeCoerce)

import           MMZKDS.Class.ArrayBased (ArrayBased(..))
import           MMZKDS.Class.DS (DS(..), DSCons(..))
import           MMZKDS.Class.List as L (List(..))
import           MMZKDS.Class.Queue (Deque(..))
import           MMZKDS.Unboxed.Base (UArrayList(..))
import           MMZKDS.Utilities
  ( arrayLengthOverflowError, expandedSize, idUArrayList, initialSize
  , outOfBoundError
  )

instance (Show a, IArray UArray a) => Show (UArrayList a) where
  show = ("ArrayList: " ++) . show . L.toList


--------------------------------------------------------------------------------
-- List Instance
--------------------------------------------------------------------------------

instance IArray UArray a => List (UArrayList a) a where
  delete :: UArrayList a -> Int -> (Maybe a, UArrayList a)
  delete al@(UArrayList l arr) index
    | index >= l || index < 0 = (Nothing, al)
    | otherwise               = ( Just (arr ! index)
                                , UArrayList (l - 1) $ 
                                  accum worker arr $ join zip [0..(l - 2)] )
    where
      pl = physicalSize al
      worker _ i
        | i < index = arr ! i
        | otherwise = arr ! (i + 1)

  get :: UArrayList a -> Int -> a
  get (UArrayList l arr) index
    | index >= l || index < 0 = outOfBoundError index
    | otherwise               = arr ! index

  indicesOf :: Eq a => UArrayList a -> a -> [Int]
  indicesOf al e
    = indicesOf' 0
    where
      l = size al
      indicesOf' i
        | i >= l          = []
        | al `get` i == e = i : indicesOf' (i + 1)
        | otherwise       = indicesOf' (i + 1)

  insert :: UArrayList a -> Int -> a -> UArrayList a
  insert al@(UArrayList l arr) index e 
    | index > l || index < 0 = outOfBoundError index
    | l == pl                = insert (resize l' al) index e 
    | otherwise              = UArrayList (l + 1) $ 
                               accum worker arr $ join zip [0..l]
    where
      pl = physicalSize al
      l' = expandedSize l
      worker _ i
        | i < index = arr ! i
        | i > index = arr ! (i - 1)
        | otherwise = e

  set :: UArrayList a -> Int -> a -> UArrayList a
  set al@(UArrayList l arr) index e
    | index >= l || index < 0 = outOfBoundError index
    | otherwise               = UArrayList l $ 
                                accum worker arr $ join zip [0..(l - 1)]
    where
      pl = physicalSize al
      worker _ i
        | i == index = e
        | otherwise  = arr ! i

  subList :: UArrayList a -> Int -> Int -> UArrayList a
  subList al@(UArrayList _ arr) inf sup 
    | sup' <= inf' = deepClear al
    | otherwise    = UArrayList len' $ 
                     accum (const ((al `get`) . (inf' +))) arr $ 
                     join zip [0..(len' - 1)]
      where
        inf' = max inf 0
        sup' = min sup (size al)
        len' = sup' - inf'
        ps   = initialSize len'

  toList :: UArrayList a -> [a]
  toList (UArrayList l arr)
    = toList' lb
    where
      (lb, _) = bounds arr
      sup     = lb + l
      toList' i
        | i == sup  = []
        | otherwise = (arr ! i) : toList' (i + 1)

  -- Overwritten default method
  deleteRange ::  UArrayList a -> Int -> Int -> ([a], UArrayList a)
  deleteRange al@(UArrayList _ arr) inf sup
    = (L.toList $ subList al inf sup, acc)
    where
      inf' = max 0 inf
      sup' = min len sup
      len  = size al
      diff = sup' - inf'
      len' = len - diff
      acc  = UArrayList len' $ accum go arr $ join zip [0..(len' - 1)]
      go e i
        | i < inf'  = e
        | otherwise = arr ! (i + diff)

  -- Overwritten default method
  insertAll :: (DSCons [a] l, DS l) => UArrayList a -> Int -> l -> UArrayList a
  insertAll al index es
    | identifier al == idUArrayList = insertAll' al index $ unsafeCoerce es
    | otherwise                     = insertAll' al index (newList $ finish es)

  -- Overwritten default method
  insertAll' :: UArrayList a -> Int -> UArrayList a -> UArrayList a
  insertAll' al@(UArrayList l arr) index al'
    | index < 0 || index > l = outOfBoundError index
    | l'' >= pl              = insertAll' (resize pl' al) index al'
    | otherwise              = UArrayList l'' $ 
                               accum go arr $ join zip [0..(l'' - 1)]
    where
      l'  = size al'
      l'' = l + l'
      pl  = physicalSize al
      pl' = expandedSize l''
      go e i
        | i < index      = e
        | i < index + l' = al' `get` (i - index)
        | otherwise      = al `get` (i - l')

  -- Overwritten default method
  lastIndexOf :: Eq a => UArrayList a -> a -> Maybe Int
  lastIndexOf al e
    = lastIndexOf' (l - 1)
    where
      l = size al
      lastIndexOf' (-1) = Nothing
      lastIndexOf' i
        | al `get` i == e = Just i
        | otherwise       = lastIndexOf' (i - 1)


--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance IArray UArray a => ArrayBased (UArrayList a) a where
  deepClear :: UArrayList a -> UArrayList a
  deepClear = const (newList [])

  newWithSize :: Foldable f => Int -> f a -> UArrayList a
  newWithSize s fl
    | s < 0     = arrayLengthOverflowError
    | otherwise = UArrayList l (array (0, s' - 1) $ zip [0..] $ F.toList fl)
    where
      l  = length fl
      s' = max s l

  physicalSize :: UArrayList a -> Int
  physicalSize (UArrayList _ arr)
    = snd (bounds arr) + 1

  resize :: Int -> UArrayList a -> UArrayList a
  resize = (. L.toList) . newWithSize


--------------------------------------------------------------------------------
-- Deque Instance
--------------------------------------------------------------------------------

instance IArray UArray a => Deque (UArrayList a) a where
  dequeueFront :: UArrayList a -> (Maybe a, UArrayList a)
  dequeueFront = popFront

  dequeueEnd :: UArrayList a -> (Maybe a, UArrayList a)
  dequeueEnd = pop

  enqueueFront :: UArrayList a -> a -> UArrayList a
  enqueueFront = push

  enqueueEnd :: UArrayList a -> a -> UArrayList a
  enqueueEnd = append


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance IArray UArray a => DS (UArrayList a) where
  clear :: UArrayList a -> UArrayList a
  clear (UArrayList _ arr)
    = UArrayList 0 arr

  identifier :: UArrayList a -> String
  identifier = const idUArrayList

  size :: UArrayList a -> Int
  size (UArrayList l _)
    = l

instance IArray UArray a => DSCons [a] (UArrayList a) where
  finish :: UArrayList a -> [a]
  finish (UArrayList l arr)
    = toList' 0
    where
      toList' i
        | i == l    = []
        | otherwise = (arr ! i) : toList' (i + 1)

  new :: [a] -> UArrayList a
  new list
    = UArrayList l (array (0, l' - 1) $ zip [0..] list)
    where
      l  = length list
      l' = initialSize l
