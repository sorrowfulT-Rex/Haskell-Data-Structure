{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.Unboxed.UArrayList where

import           Control.Monad (join)
import           Data.Array.Unboxed (UArray(..), accumArray, array, bounds, (!))
import           Data.Foldable (toList)

import           MMZKDS.ArrayBased (ArrayBased(..))
import           MMZKDS.List (List(..))
import           MMZKDS.Utilities 
  (arrayLengthOverflowError, expandedSize, initialSize, outOfBoundError)

-- | @UArrayList@ is a data structure implementing the 'List' class with an
-- internal array storing unboxed values.
-- All operations that requires mutation on the @UArrayList@ (exept @clear@ and
-- @deepClear@) requires generating a new @UArrayList@, which is very costly 
-- (always O(n)). Therefore it is recommended to use the mutable version
-- 'MArrayList' for frequent state updates.
--
data UArrayList e = UArrayList {-# UNPACK #-} !Int (UArray Int e)

instance Show a => Show (UArrayList a) where
  show = ("ArrayList: " ++) . show . toList

instance Foldable UArrayList where
  foldr f b al
    = foldr f b $ toList al

  length (UArrayList l _)
    = l

  toList (UArrayList l arr)
    = take l $ toList arr


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance List UArrayList where
  add :: Int -> a -> UArrayList a -> UArrayList a
  add index e al@(UArrayList l arr)
    | index > l || index < 0 = outOfBoundError index
    | l == pl                = add index e (resize l' al)
    | otherwise 
      = UArrayList (l + 1) 
        $ accumArray worker undefined (0, pl - 1) $ join zip [0..l]
    where
      pl = physicalSize al
      l' = expandedSize l
      worker _ i
        | i < index = arr ! i
        | i > index = arr ! (i - 1)
        | otherwise = e

  clear :: UArrayList a -> UArrayList a
  clear (UArrayList l arr)
    = UArrayList 0 arr

  delete :: Int -> UArrayList a -> (Maybe a, UArrayList a)
  delete index al@(UArrayList l arr)
    | index >= l || index < 0 = (Nothing, al)
    | otherwise               = (Just (arr ! index), UArrayList (l - 1) 
        $ accumArray worker undefined (0, pl - 1) $ join zip [0..(l - 2)])
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
  
  newList :: Foldable f => f a -> UArrayList a
  newList fl
    = UArrayList l (array (0, l' - 1) $ zip [0..] $ toList fl)
    where
      l  = length fl
      l' = initialSize l

  set :: UArrayList a -> Int -> a -> UArrayList a
  set al@(UArrayList l arr) index e
    | index >= l || index < 0 = outOfBoundError index
    | otherwise               = UArrayList l 
        $ accumArray worker undefined (0, pl - 1) $ join zip [0..(l - 1)]
    where
      pl = physicalSize al
      worker _ i
        | i == index = e
        | otherwise  = arr ! i
  
  size :: UArrayList a -> Int
  size (UArrayList l _) 
    = l

  subList :: Int -> Int -> UArrayList a -> UArrayList a
  subList inf sup al
    | sup' <= inf' = deepClear al
    | otherwise    = UArrayList len'
        $ accumArray worker undefined (0, ps - 1) $ join zip [0..(len' - 1)]
      where
        inf' = max inf 0
        sup' = min sup (size al)
        len' = sup' - inf'
        ps   = initialSize len'
        worker _ i
          = al `get` (i + inf')

  -- Overwritten default methods
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

instance ArrayBased UArrayList where
  deepClear :: UArrayList a -> UArrayList a
  deepClear = const (newList [])

  newWithSize :: Foldable f => Int -> f a -> UArrayList a
  newWithSize s fl
    | s < 0     = arrayLengthOverflowError
    | otherwise = UArrayList l (array (0, s' - 1) $ zip [0..] $ toList fl)
    where
      l  = length fl
      s' = max s l

  physicalSize :: UArrayList a -> Int
  physicalSize (UArrayList _ arr)
    = snd (bounds arr) + 1


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

foo :: IO ()
foo = do
  al  <- return $ (newList [4, 3, 2, 1] :: UArrayList Int)
  print $ sort al
  