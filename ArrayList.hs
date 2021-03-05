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
  show (ArrayList l arr)
    = "ArrayList: " ++ show (take l $ toList arr)

instance Foldable ArrayList where
  foldr f b (ArrayList _ arr)
    = foldr f b arr
  length (ArrayList l _)
    = l

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
      l' = (3 * l) `div` 2
      worker _ i
        | i < index = arr ! i
        | i > index = arr ! (i - 1)
        | otherwise = e

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
  let arrayList = newList [1..10] :: ArrayList Int
  print $ arrayList
  (_, arrayList') <- return $ remove 6 arrayList
  (bruh, arrayList') <- return $ popEnd arrayList'
  print bruh
  arrayList' <- return $ pop arrayList'
  print $ arrayList'
