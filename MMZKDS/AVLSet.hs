{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.AVLSet (AVLSet) where

import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.List (foldl')
import           Data.Maybe (isJust)

import           MMZKDS.Base (AVLSet(..))
import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.PriorityQueue (PriorityQueue(..))
import           MMZKDS.Set as S (Set(add, contains, findAny, remove))
import           MMZKDS.Utilities
  (GBT(..), GBTN(..), addBT, containsBT, depthBTN, emptyGBT, rootBT, removeBT, 
   removeMinBT, rotateLeftGBTN, rotateRightGBTN
  )

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
-- PriorityQueue Instance
--------------------------------------------------------------------------------

instance Ord a => PriorityQueue AVLSet a where
  add :: a -> AVLSet a -> AVLSet a
  add = S.add

  pop :: AVLSet a -> (Maybe a, AVLSet a)
  pop = removeMinBT balanceAVL


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
  finish = toList

  new :: [a] -> AVLSet a
  new = foldl' (flip S.add) $ coerce (emptyGBT :: GBT a)


--------------------------------------------------------------------------------
-- AVL-Tree Specific Function
--------------------------------------------------------------------------------

-- | Utility Function.
-- A balancing function that re-balances the tree. It will be called by @add@
-- and @remove@.
-- 
balanceAVL :: Ord a => GBTN a -> GBTN a
balanceAVL tree@(GBNode _ l e r)
  | abs (depthBTN l - depthBTN r) <= 1 = tree
balanceAVL tree@(GBNode d l@(GBNode ld ll _ lr) e r)
  | leftImb && llImb = rotateRightGBTN tree
  | leftImb          = rotateRightGBTN $ GBNode d (rotateLeftGBTN l) e r
  where
    leftImb = ld > depthBTN r
    llImb   = depthBTN ll >= depthBTN lr
balanceAVL tree@(GBNode d l e r@(GBNode rd rl _ rr))
  | rightImb && rrImb = rotateLeftGBTN tree
  | rightImb          = rotateLeftGBTN $ GBNode d l e (rotateRightGBTN r)
  where
    rightImb = rd > depthBTN l
    rrImb    = depthBTN rr >= depthBTN rl
balanceAVL tree
  = tree
