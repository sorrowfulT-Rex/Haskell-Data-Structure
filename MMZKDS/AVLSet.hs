{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.AVLSet where

import           Data.Foldable as F (toList)
import           Data.List (foldl')
import           Data.Maybe (isJust)

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.Set (Set(..))
import           MMZKDS.Utilities
  (GenericBinaryTree(..), addGenericBinaryTree, containsGenericBinaryTree,
   depthGenericBinaryTree, rootGenericBinaryTree, removeGenericBinaryTree
  )

-- | 
-- It is expected that the type of its elements is an instance of 'Ord'.
-- It has O(log n) adding, O(log n) deleting, O(log n) searching, O(n * log n) 
-- union and intersection, and O(n * log n) construction from list.
-- 
data AVLSet e = AVLSet {-# UNPACK #-} !Int (GenericBinaryTree e)

instance forall a. (Ord a, Show a) => Show (AVLSet a) where
  show = ("Set: " ++) . show . (finish :: AVLSet a -> [a])


--------------------------------------------------------------------------------
-- Set Instance
--------------------------------------------------------------------------------

instance Ord a => Set AVLSet a where
  add :: a -> AVLSet a -> AVLSet a
  add e set@(AVLSet n tree)
    | isNotIn   = AVLSet (n + 1) tree'
    | otherwise = set
    where
      (isNotIn, tree') = addGenericBinaryTree e tree
  contains :: AVLSet a -> a -> Bool
  contains (AVLSet _ tree) e
    = containsGenericBinaryTree tree e
  
  findAny :: AVLSet a -> Maybe a
  findAny (AVLSet _ tree)
    = rootGenericBinaryTree tree

  remove :: a -> AVLSet a -> (Maybe a, AVLSet a)
  remove e set@(AVLSet n tree)
    | isJust me = (me, AVLSet (n - 1) tree')
    | otherwise = (me, set)
    where
      (me, tree') = removeGenericBinaryTree e tree


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance DS (AVLSet a) where
  clear :: AVLSet a -> AVLSet a
  clear = const $ AVLSet 0 GBEmpty 

  size :: AVLSet a -> Int
  size (AVLSet n _)
    = n

instance Ord a => DSCons [a] (AVLSet a) where
  finish :: AVLSet a -> [a]
  finish (AVLSet _ tree)
    = F.toList tree

  new :: [a] -> AVLSet a
  new = foldl' (flip add) (clear undefined)


--------------------------------------------------------------------------------
-- AVL-Tree Specific Functions
--------------------------------------------------------------------------------
