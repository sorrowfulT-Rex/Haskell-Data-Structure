{-# LANGUAGE InstanceSigs #-}

module ArrayList where

import           Control.Monad
import           Data.Array
import           Data.Bits
import           Data.Foldable

import           ArrayBased
import           List

data ArrayList e = ArrayList !Int (Array Int e)

instance Show a => Show (ArrayList a) where
  show = ("ArrayList: " ++) . show . toList

instance Foldable ArrayList where
  foldr f b (ArrayList _ arr)
    = foldr f b $ toList arr

  length (ArrayList l _)
    = l

  toList (ArrayList l arr)
    = take l $ toList arr

outOfBoundError :: Int -> a
outOfBoundError i
  = error $ "Index " ++ show i ++ " is out of bound!"


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
      l' = 1 + (3 * l) `div` 2
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
    | otherwise = arr ! index

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
  
  size :: ArrayList a -> Int
  size (ArrayList l _) 
    = l

  newList :: Foldable f => f a -> ArrayList a
  newList fl
    = ArrayList l (array (0, l' - 1) $ zip [0..] $ toList fl)
    where
      l  = length fl
      l' = shiftL 1 (ceiling $ logBase 2 (fromIntegral $ l + 1))
  

--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance ArrayBased ArrayList where
  deepClear :: ArrayList a -> ArrayList a
  deepClear = const (newList [])

  newWithSize :: Foldable f => Int -> f a -> ArrayList a
  newWithSize s fl
    = ArrayList l (array (0, s' - 1) $ zip [0..] $toList fl)
    where
      l  = length fl
      s' = max s l

  physicalSize :: ArrayList a -> Int
  physicalSize (ArrayList _ arr)
    = snd (bounds arr) + 1

  resize :: Int -> ArrayList a -> ArrayList a
  resize s mal
    = newWithSize s (take (length mal) (toList mal))


--------------------------------------------------------------------------------
-- Playground
--------------------------------------------------------------------------------

foo :: IO ()
foo = do
  let al = newList [1..10] :: ArrayList Int
  print $ al
  (_, al') <- return $ remove 6 al
  (v, al') <- return $ popEnd al'
  print v
  al'      <- return $ pop al'
  print al'
  print $ (snd al') `get` 6
  al'      <- return $ deepClear (snd al')
  print al'
  print $ physicalSize al'
