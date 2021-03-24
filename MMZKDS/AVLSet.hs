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
  (GBT(..), GBTN(..), addBT, containsBT, depthBTN, emptyGBT, rootBT, removeBT, rotateRightGBTN, rotateLeftGBTN)

-- | An immutable set structure implemented with an internal AVL-tree.
-- It is expected that the type of its elements is an instance of 'Ord'.
-- It has O(log n) adding, O(log n) deleting, O(log n) searching, O(n * log n) 
-- union and intersection, and O(n * log n) construction from list.
-- 
newtype AVLSet e = AVLSet (GBT e)

instance Show a => Show (AVLSet a) where
  show (AVLSet tree)
    = show tree


--------------------------------------------------------------------------------
-- Set Instance
--------------------------------------------------------------------------------

instance Ord a => Set AVLSet a where
  add :: a -> AVLSet a -> AVLSet a
  add = addBT balanceAVL

  contains :: AVLSet a -> a -> Bool
  contains = containsBT
  
  findAny :: AVLSet a -> Maybe a
  findAny = rootBT

  remove :: a -> AVLSet a -> (Maybe a, AVLSet a)
  remove = removeBT balanceAVL


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
  new = foldl' (flip add) $ coerce (emptyGBT :: GBT a)


--------------------------------------------------------------------------------
-- AVL-Tree Specific Functions
--------------------------------------------------------------------------------

balanceAVL :: Ord a => GBTN a -> GBTN a
balanceAVL tree@(GBNode _ l e r)
  | abs (depthBTN l - depthBTN r) <= 1 = tree
balanceAVL tree@(GBNode d l@(GBNode ld ll le lr) e r)
  | leftImb && llImb = rotateRightGBTN tree
  | leftImb          = rotateRightGBTN $ GBNode d (rotateLeftGBTN ll) e r
  where
    leftImb = ld > depthBTN r
    llImb   = depthBTN ll >= depthBTN lr
balanceAVL tree@(GBNode d l e r@(GBNode rd rl re rr))
  | rightImb && rrImb = rotateLeftGBTN tree
  | rightImb          = rotateLeftGBTN $ GBNode d l e (rotateRightGBTN r)
  where
    rightImb = rd > depthBTN l
    rrImb    = depthBTN rr >= depthBTN rl
balanceAVL tree
  = tree
