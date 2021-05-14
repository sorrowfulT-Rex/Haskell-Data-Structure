{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.ArrayList (ArrayList) where

import           Control.Monad (join)
import           Data.Array (Array, accumArray, array, bounds, (!))
import           Data.Foldable as F (toList)
import           Data.Maybe (isJust)

import           MMZKDS.ArrayBased (ArrayBased(..))
import           MMZKDS.Base (ArrayList(..))
import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.Queue (Deque(..))
import           MMZKDS.List as L (List(..))
import           MMZKDS.Utilities
  ( arrayLengthOverflowError, expandedSize, idArrayList, initialSize
  , outOfBoundError
  )

instance Show a => Show (ArrayList a) where
  show = ("ArrayList: " ++) . show . L.toList


--------------------------------------------------------------------------------
-- List Instance
--------------------------------------------------------------------------------

instance List (ArrayList a) a where
  delete :: Int -> ArrayList a -> (Maybe a, ArrayList a)
  delete index al@(ArrayList l arr)
    | index >= l || index < 0 = (Nothing, al)
    | otherwise               = (Just (arr ! index), ArrayList (l - 1)
        $ accumArray worker undefined (0, pl - 1) $ join zip [0..(l - 2)])
    where
      pl = physicalSize al
      worker _ i
        | i < index = arr ! i
        | otherwise = arr ! (i + 1)

  get :: ArrayList a -> Int -> a
  get (ArrayList l arr) index
    | index >= l || index < 0 = outOfBoundError index
    | otherwise               = arr ! index

  indicesOf :: Eq a => ArrayList a -> a -> [Int]
  indicesOf al e
    = indicesOf' 0
    where
      l = size al
      indicesOf' i
        | i >= l          = []
        | al `get` i == e = i : indicesOf' (i + 1)
        | otherwise       = indicesOf' (i + 1)

  insert :: Int -> a -> ArrayList a -> ArrayList a
  insert index e al@(ArrayList l arr)
    | index > l || index < 0 = outOfBoundError index
    | l == pl                = insert index e (resize l' al)
    | otherwise
      = ArrayList (l + 1)
        $ accumArray worker undefined (0, pl - 1) $ join zip [0..l]
    where
      pl = physicalSize al
      l' = expandedSize l
      worker _ i
        | i < index = arr ! i
        | i > index = arr ! (i - 1)
        | otherwise = e

  set :: ArrayList a -> Int -> a -> ArrayList a
  set al@(ArrayList l arr) index e
    | index >= l || index < 0 = outOfBoundError index
    | otherwise               = ArrayList l
        $ accumArray worker undefined (0, pl - 1) $ join zip [0..(l - 1)]
    where
      pl = physicalSize al
      worker _ i
        | i == index = e
        | otherwise  = arr ! i

  subList :: Int -> Int -> ArrayList a -> ArrayList a
  subList inf sup al
    | sup' <= inf' = deepClear al
    | otherwise    = ArrayList len'
        $ accumArray worker undefined (0, ps - 1) $ join zip [0..(len' - 1)]
      where
        inf' = max inf 0
        sup' = min sup (size al)
        len' = sup' - inf'
        ps   = initialSize len'
        worker _ i
          = al `get` (i + inf')

  toList (ArrayList l arr)
    = take l $ F.toList arr

  -- Overwritten default method
  lastIndexOf :: Eq a => ArrayList a -> a -> Maybe Int
  lastIndexOf al e
    = lastIndexOf' (l - 1)
    where
      l = size al
      lastIndexOf' (-1) = Nothing
      lastIndexOf' i
        | al `get` i == e = Just i
        | otherwise       = lastIndexOf' (i - 1)


--------------------------------------------------------------------------------
-- ArrayBased Instance
--------------------------------------------------------------------------------

instance ArrayBased (ArrayList a) a where
  deepClear :: ArrayList a -> ArrayList a
  deepClear = const (newList [])

  newWithSize :: Foldable f => Int -> f a -> ArrayList a
  newWithSize s fl
    | s < 0     = arrayLengthOverflowError
    | otherwise = ArrayList l (array (0, s' - 1) $ zip [0..] $ F.toList fl)
    where
      l  = length fl
      s' = max s l

  physicalSize :: ArrayList a -> Int
  physicalSize (ArrayList _ arr)
    = snd (bounds arr) + 1

  resize :: Int -> ArrayList a -> ArrayList a
  resize = (. L.toList) . newWithSize


--------------------------------------------------------------------------------
-- Deque Instance
--------------------------------------------------------------------------------

instance Deque (ArrayList a) a where
  dequeueFront :: ArrayList a -> (Maybe a, ArrayList a)
  dequeueFront = popFront

  dequeueEnd :: ArrayList a -> (Maybe a, ArrayList a)
  dequeueEnd = pop

  enqueueFront :: a -> ArrayList a -> ArrayList a
  enqueueFront = push

  enqueueEnd :: a -> ArrayList a -> ArrayList a
  enqueueEnd = append


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance DS (ArrayList a) where
  clear :: ArrayList a -> ArrayList a
  clear (ArrayList _ arr)
    = ArrayList 0 arr

  identifier :: ArrayList a -> String
  identifier = const idArrayList

  size :: ArrayList a -> Int
  size (ArrayList l _)
    = l

instance DSCons [a] (ArrayList a) where
  finish :: ArrayList a -> [a]
  finish = L.toList

  new :: [a] -> ArrayList a
  new list
    = ArrayList l (array (0, l' - 1) $ zip [0..] list)
    where
      l  = length list
      l' = initialSize l


--------------------------------------------------------------------------------
-- Foldable Instance
--------------------------------------------------------------------------------

instance Foldable ArrayList where
  foldr :: (a -> b -> b) -> b -> ArrayList a -> b
  foldr f v (ArrayList l arr) 
    = go 0 v
    where
      go i v
        | i == l    = v
        | otherwise = f (arr ! i) (go (i + 1) v)

  elem :: Eq a => a -> ArrayList a -> Bool
  elem = (isJust .) . flip indexOf

  null :: ArrayList a -> Bool
  null = isNull

  length :: ArrayList a -> Int
  length = size

  toList :: ArrayList a -> [a]
  toList = L.toList
