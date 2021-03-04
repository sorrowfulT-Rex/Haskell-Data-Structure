{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe
import           Data.Foldable

import           ArrayBased
import           List

data MArrayList e = MArrayList !Int (Array Int e)

instance Show a => Show (MArrayList a) where
  show (MArrayList l arr)
    = "MArrayList: " ++ show (take l $ toList arr)

instance Foldable MArrayList where
  foldr f b (MArrayList _ arr)
    = foldr f b arr
  length (MArrayList l _)
    = l


--------------------------------------------------------------------------------
-- List Functions
--------------------------------------------------------------------------------

instance List MArrayList where
  add :: forall a. Int -> a -> MArrayList a -> MArrayList a
  add index e mal@(MArrayList l arr)
    | index > l || index < 0 = error "Index out of bound!"
    | l == physicalSize mal  = add index e (resize l' mal)
    | otherwise              = MArrayList (l + 1) $ runST $ do
      arrST <- unsafeThaw arr :: ST s (STArray s Int a)
      addST index e (l - 1) arrST
      unsafeFreeze arrST
    where
      l' = (3 * l) `div` 2
  
  size :: MArrayList a -> Int
  size (MArrayList l _) 
    = l

  newList :: Foldable f => f a -> MArrayList a
  newList fl
    = MArrayList l (array (0, l - 1) $ zip [0..] $ toList fl)
    where
      l = length fl
  

--------------------------------------------------------------------------------
-- ArrayBased Functions
--------------------------------------------------------------------------------

instance ArrayBased MArrayList where
  physicalSize :: MArrayList a -> Int
  physicalSize (MArrayList _ arr)
    = 1 + snd (bounds arr)

  newWithSize :: Foldable f => Int -> f a -> MArrayList a
  newWithSize s fl
    = MArrayList l (array (0, s' - 1) $ zip [0..] $toList fl)
    where
      l  = length fl
      s' = max s l

  resize :: Int -> MArrayList a -> MArrayList a
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

main :: IO ()
main = do
  let arrayList = newList [1..10] :: MArrayList Int
  print $ arrayList
  let arrayList' = push 11 arrayList
  print $ arrayList'
  print $ arrayList
