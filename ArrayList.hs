{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ArrayList where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
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


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance List ArrayList where
  add :: forall a. Int -> a -> ArrayList a -> ArrayList a
  add index e mal@(ArrayList l arr)
    | index > l || index < 0 = error "Index out of bound!"
    | l == physicalSize mal  = add index e (resize l' mal)
    | otherwise              = ArrayList (l + 1) $ runST $ do
      arrST <- thaw arr :: ST s (STArray s Int a)
      addST index e (l - 1) arrST
      freeze arrST
    where
      l' = (3 * l) `div` 2
  
  size :: ArrayList a -> Int
  size (ArrayList l _) 
    = l

  newList :: Foldable f => f a -> ArrayList a
  newList fl
    = ArrayList l (array (0, l - 1) $ zip [0..] $ toList fl)
    where
      l = length fl
  

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

foo :: IO ()
foo = do
  let arrayList = newList [1..10] :: ArrayList Int
  print $ arrayList
  let arrayList' = push 11 arrayList
  print $ arrayList'
  print $ arrayList
