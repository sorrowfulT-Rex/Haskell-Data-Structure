{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ArrayList where

import           Control.Monad (join)
import           Data.Array (Array(..), accumArray, array, bounds, (!))
import           Data.Foldable (toList)

import           ArrayBased (ArrayBased(..), arrayLengthOverflowError)
import           List 
  (List(..), expandedSize, initialSize, outOfBoundError)

data ArrayList e = ArrayList {-# UNPACK #-} !Int (Array Int e)

instance Show a => Show (ArrayList a) where
  show = ("ArrayList: " ++) . show . toList

instance Foldable ArrayList where
  foldr f b al
    = foldr f b $ toList al

  length (ArrayList l _)
    = l

  toList (ArrayList l arr)
    = take l $ toList arr


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance List ArrayList where
  add :: Int -> a -> ArrayList a -> ArrayList a
  add index e al@(ArrayList l arr)
    | index > l || index < 0 = outOfBoundError index
    | l == pl                = add index e (resize l' al)
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

  clear :: ArrayList a -> ArrayList a
  clear (ArrayList l arr)
    = ArrayList 0 arr

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
  
  newList :: Foldable f => f a -> ArrayList a
  newList fl
    = ArrayList l (array (0, l' - 1) $ zip [0..] $ toList fl)
    where
      l  = length fl
      l' = initialSize l

  remove :: Int -> ArrayList a -> (Maybe a, ArrayList a)
  remove index al@(ArrayList l arr)
    | index >= l || index < 0 = (Nothing, al)
    | otherwise
      = (Just (arr ! index), ArrayList (l - 1) 
        $ accumArray worker undefined (0, pl - 1) $ join zip [0..(l - 2)])
    where
      pl = physicalSize al
      worker _ i
        | i < index = arr ! i
        | otherwise = arr ! (i + 1)

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
  
  size :: ArrayList a -> Int
  size (ArrayList l _) 
    = l


--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance ArrayBased ArrayList where
  deepClear :: ArrayList a -> ArrayList a
  deepClear = const (newList [])

  newWithSize :: Foldable f => Int -> f a -> ArrayList a
  newWithSize s fl
    | s < 0     = arrayLengthOverflowError
    | otherwise = ArrayList l (array (0, s' - 1) $ zip [0..] $ toList fl)
    where
      l  = length fl
      s' = max s l

  physicalSize :: ArrayList a -> Int
  physicalSize (ArrayList _ arr)
    = snd (bounds arr) + 1


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

foo :: IO ()
foo = do
  let al = newList [] :: ArrayList Int
  al' <- return $ append 3 al
  al' <- return $ append 4 al'
  print al'
  