{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.AVLSet (AVLSet) where

import           Data.Bool (bool)
import           Data.Foldable as F (toList)
import           Data.List (foldl')
import           Data.Maybe (isJust)

import           MMZKDS.Base (AVLSet(..))
import           MMZKDS.Class.DS (DS(..), DSCons(..))
import           MMZKDS.PriorityQueue (PriorityQueue(..))
import           MMZKDS.Set as S (Set(..))
import           MMZKDS.Utilities (idAVLSet)


--------------------------------------------------------------------------------
-- Set Instance
--------------------------------------------------------------------------------

instance Ord a => Set (AVLSet a) a where
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

instance Ord a => PriorityQueue (AVLSet a) a where
  add :: AVLSet a -> a -> AVLSet a
  add = flip S.add

  pop :: AVLSet a -> (Maybe a, AVLSet a)
  pop = removeMinBT balanceAVL


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance DS (AVLSet a) where
  clear :: AVLSet a -> AVLSet a
  clear = const AVLEmpty

  identifier :: AVLSet a -> String
  identifier = const idAVLSet

  size :: AVLSet a -> Int
  size AVLEmpty            = 0
  size (AVLLeaf _)         = 1
  size (AVLNode s _ _ _ _) = s

instance Ord a => DSCons [a] (AVLSet a) where
  finish :: AVLSet a -> [a]
  finish = F.toList

  new :: [a] -> AVLSet a
  new = foldl' (flip S.add) AVLEmpty


--------------------------------------------------------------------------------
-- Foldable Instance
--------------------------------------------------------------------------------

instance Foldable AVLSet where
  foldr f e tree = case tree of
    AVLEmpty           -> e
    AVLLeaf e'         -> f e' e
    AVLNode _ _ l e' r -> foldr f (f e' (foldr f e r)) l

  toList = toList' []
    where
      toList' [] AVLEmpty
        = []
      toList' ((AVLNode _ _ _ e r) : stack) AVLEmpty
        = e : toList' stack r
      toList'  [] (AVLLeaf e)
        = [e]
      toList' ((AVLNode _ _ _ e' r) : stack) (AVLLeaf e)
        = e : e' : toList' stack r
      toList' stack bt@(AVLNode _ _ l _ _)
        = toList' (bt : stack) l


--------------------------------------------------------------------------------
-- AVL-Tree Specific Function
--------------------------------------------------------------------------------

-- | Adds an element to the AVL-tree with a balancing function.
-- If the element exists already, the function replaces it and returns @False@.
-- Otherwise returns @True@.
-- 
addBT :: Ord a => (AVLSet a -> AVLSet a) -> a -> AVLSet a -> AVLSet a
addBT f e set
  = snd $ addAVLSet e set
  where
    addSize          = bool 0 1
    (isNotIn, tree') = addAVLSet e set
    addAVLSet e tree   = case tree of
      AVLEmpty   -> (True, AVLLeaf e)
      AVLLeaf e' -> addLeaf e'
      _          -> addNode tree
    addLeaf e'
      | e < e'    = (True, AVLNode 2 2 (AVLLeaf e) e' AVLEmpty)
      | e > e'    = (True, AVLNode 2 2 AVLEmpty e' (AVLLeaf e))
      | otherwise = (False, AVLLeaf e)
    addNode (AVLNode s d l e' r)
      | e < e'    
        = let (b, subT) = addAVLSet e l
              d' = 1 + max (depthBTN r) (depthBTN subT)
          in  (b, f $ AVLNode (s + addSize b) d' subT e' r)
      | e > e'    
        = let (b, subT) = addAVLSet e r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in  (b, f $ AVLNode (s + addSize b) d' l e' subT)
      | otherwise = (False, AVLNode s d l e r)

-- | Utility Function.
-- A balancing function that re-balances the tree. It will be called by @add@
-- and @remove@.
-- 
balanceAVL :: Ord a => AVLSet a -> AVLSet a
balanceAVL tree@(AVLNode _ _ l e r)
  | abs (depthBTN l - depthBTN r) <= 1 = tree
balanceAVL tree@(AVLNode s d l@(AVLNode _ ld ll _ lr) e r)
  | leftImb && llImb = rotateRightAVLSet tree
  | leftImb          = rotateRightAVLSet $ AVLNode s d (rotateLeftAVLSet l) e r
  where
    leftImb = ld > depthBTN r
    llImb   = depthBTN ll >= depthBTN lr
balanceAVL tree@(AVLNode s d l e r@(AVLNode _ rd rl _ rr))
  | rightImb && rrImb = rotateLeftAVLSet tree
  | rightImb          = rotateLeftAVLSet $ AVLNode s d l e (rotateRightAVLSet r)
  where
    rightImb = rd > depthBTN l
    rrImb    = depthBTN rr >= depthBTN rl
balanceAVL tree
  = tree

-- | Utility Function.
-- Tests if the element is in the the AVL-tree.
-- 
containsBT :: Ord a => AVLSet a -> a -> Bool
containsBT set e = case searchAVLSet e set of
  AVLEmpty -> False
  _        -> True

-- | Utility Function.
-- Returns the depth of the the AVL-tree.
-- 
depthBTN :: Ord a => AVLSet a -> Int
depthBTN tree = case tree of
  AVLNode _ d _ _ _ -> d
  AVLLeaf _         -> 1
  _                 -> 0

-- | Utility Function.
-- Reduce a @AVLNode@ to @AVLLeaf@ if both children are @AVLEmpty@.
-- 
normAVLSet :: AVLSet a -> AVLSet a
normAVLSet (AVLNode _ _ AVLEmpty e AVLEmpty)
  = AVLLeaf e
normAVLSet tree 
  = tree

-- | Utility Function.
-- Removes the given element from the the AVL-tree with a balancing function.
-- Returns a tuple consisting of the removed element (or Nothing, if the
-- element is not in the tree), and the tree without the element.
--
removeBT :: Ord a
         => (AVLSet a -> AVLSet a) 
         -> a 
         -> AVLSet a 
         -> (Maybe a, AVLSet a)
removeBT f e set
  = removeAVLSet set
  where
    delSize     = bool 0 (-1) . isJust
    (me, tree') = removeAVLSet set
    removeAVLSet tree = case tree of
      AVLEmpty   -> (Nothing, tree)
      AVLLeaf e' -> removeLeaf e'
      _         -> removeNode tree
    removeLeaf e'
      | e == e'   = (Just e', AVLEmpty) 
      | otherwise = (Nothing, AVLLeaf e') 
    removeNode (AVLNode _ _ AVLEmpty e' AVLEmpty) 
      = removeAVLSet (AVLLeaf e')
    removeNode (AVLNode s d l e' r)
      | e < e'       
        = let (me, subT) = removeAVLSet l
              d' = 1 + max (depthBTN r) (depthBTN subT)
          in  (me, f $ normAVLSet $ AVLNode (s + delSize me) d' subT e' r)
      | e > e' 
        = let (me, subT) = removeAVLSet r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in  (me, f $ normAVLSet $ AVLNode (s + delSize me) d' l e' subT)
      | AVLEmpty <- l = (Just e', r)
      | AVLEmpty <- r = (Just e', l)
      | otherwise    
        = let (Just eSucc, subT) = removeMinAVLSet f r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in  (Just e', f $ normAVLSet $ AVLNode (s - 1) d' l eSucc subT)

-- | Utility Function.
-- Removing the minimum (most left) element from the the AVL-tree with a 
-- balancing function.
-- 
removeMinBT :: Ord a 
            => (AVLSet a -> AVLSet a) 
            -> AVLSet a 
            -> (Maybe a, AVLSet a)
removeMinBT f set
  = (me, set')
  where
    (me, set') = removeMinAVLSet f set

-- | Utility Function.
-- Removing the minimum (most left) element from the AVL-Tree with a 
-- balancing function.
-- 
removeMinAVLSet :: Ord a 
                => (AVLSet a -> AVLSet a) 
                -> AVLSet a 
                -> (Maybe a, AVLSet a)
removeMinAVLSet f (AVLNode _ d AVLEmpty e r)
  = (Just e, r)
removeMinAVLSet f (AVLNode s d l e r)
  = let (me, subT) = removeMinAVLSet f l 
        d' = 1 + max (depthBTN r) (depthBTN subT)
    in  (me, 
        f $ normAVLSet $ AVLNode (s - if isJust me then 1 else 0) d' subT e r)
removeMinAVLSet _ (AVLLeaf e)
  = (Just e, AVLEmpty)
removeMinAVLSet _ _ 
  = (Nothing, AVLEmpty)

-- | Utility Function.
-- Returns the root element of the AVL-Tree.
-- 
rootBT :: (Ord a) => AVLSet a -> Maybe a
rootBT set = case set of
  AVLEmpty          -> Nothing
  AVLLeaf e         -> Just e
  AVLNode _ _ _ e _ -> Just e

-- | Utility Function.
-- Rotate the AVL-Tree to the left with the given node as the root.
-- 
rotateLeftAVLSet :: Ord a => AVLSet a -> AVLSet a
rotateLeftAVLSet (AVLNode s _ l e (AVLNode _ _ rl re rr))
  = AVLNode s (1 + max (depthBTN ll) (depthBTN rr)) ll re rr
  where
    s' = 1 + sizeAVLSet l + sizeAVLSet rl
    ll = normAVLSet $ AVLNode s' (1 + max (depthBTN l) (depthBTN rl)) l e rl
rotateLeftAVLSet (AVLNode s _ l e (AVLLeaf re))
  = let dl = depthBTN l 
    in  AVLNode s (1 + dl) 
        (normAVLSet $ AVLNode (s - 1) dl l e AVLEmpty) re AVLEmpty
rotateLeftAVLSet tree
  = tree

-- | Utility Function.
-- Rotate the tree to the right with the given node as the root.
-- Works for the Generic Binary Tree Node data type @AVLSet@.
-- 
rotateRightAVLSet :: Ord a => AVLSet a -> AVLSet a
rotateRightAVLSet (AVLNode s _ (AVLNode _ _ ll le lr) e r)
  = AVLNode s (1 + max (depthBTN ll) (depthBTN rr)) ll le rr
  where
    s' = 1 + sizeAVLSet r + sizeAVLSet lr
    rr = normAVLSet $ AVLNode s' (1 + max (depthBTN r) (depthBTN lr)) lr e r
rotateRightAVLSet (AVLNode s _ (AVLLeaf le) e r)
  = let dl = depthBTN r
    in  AVLNode s (1 + dl) AVLEmpty le 
        (normAVLSet $ AVLNode (s - 1) dl AVLEmpty e r) 
rotateRightAVLSet tree
  = tree

-- | Utility Function.
-- Searches for the sub-tree with the given element as the root.
-- 
searchAVLSet :: Ord a => a -> AVLSet a -> AVLSet a
searchAVLSet e
  = search'
  where
    search' AVLEmpty
      = AVLEmpty
    search' t@(AVLLeaf e')
      = if e == e' then t else AVLEmpty
    search' t@(AVLNode _ _ l e' r)
      | e < e'    = search' l
      | e > e'    = search' r
      | otherwise = t

-- | Utility Function.
-- Returns the size (number of elements) of the AVL-Tree.
-- 
sizeAVLSet :: AVLSet a -> Int
sizeAVLSet tree = case tree of
  AVLNode s _ _ _ _ -> s
  AVLLeaf _         -> 1
  _                 -> 0
