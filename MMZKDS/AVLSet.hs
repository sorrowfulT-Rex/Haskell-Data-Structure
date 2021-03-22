{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.AVLSet where

import           Data.Coerce (coerce)
import           Data.Foldable as F (toList)
import           Data.List (foldl')
import           Data.Maybe (isJust)

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.Set (Set(..))
import           MMZKDS.Utilities
  (GBT(..), addBT, containsBT, emptyGBT, rootBT, removeBT)

-- | An immutable set structure implemented with an internal AVL-tree.
-- It is expected that the type of its elements is an instance of 'Ord'.
-- It has O(log n) adding, O(log n) deleting, O(log n) searching, O(n * log n) 
-- union and intersection, and O(n * log n) construction from list.
-- 
newtype AVLSet e = AVLSet (GBT e)

instance forall a. (Ord a, Show a) => Show (AVLSet a) where
  show = ("Set: " ++) . show . (finish :: AVLSet a -> [a])


--------------------------------------------------------------------------------
-- Set Instance
--------------------------------------------------------------------------------

instance Ord a => Set AVLSet a where
  add :: a -> AVLSet a -> AVLSet a
  add = addBT

  contains :: AVLSet a -> a -> Bool
  contains = containsBT
  
  findAny :: AVLSet a -> Maybe a
  findAny = rootBT

  remove :: a -> AVLSet a -> (Maybe a, AVLSet a)
  remove = removeBT


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance DS (AVLSet a) where
  clear :: AVLSet a -> AVLSet a
  clear = const $ coerce (emptyGBT :: GBT a)

  size :: AVLSet a -> Int
  size = length

instance Ord a => DSCons [a] (AVLSet a) where
  finish :: AVLSet a -> [a]
  finish = F.toList

  new :: [a] -> AVLSet a
  new = foldl' (flip add) (clear undefined)


--------------------------------------------------------------------------------
-- AVL-Tree Specific Functions
--------------------------------------------------------------------------------
