{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.Utilities where

import           Data.Bits (shiftL)
import           Data.Bool (bool)
import           Data.Coerce (Coercible, coerce)
import           Data.Foldable (toList)
import           Data.Maybe (isJust)

 
--------------------------------------------------------------------------------
-- Array & STArray
--------------------------------------------------------------------------------

-- | Utility Function. 
-- Returns an error indicating the length of the array has exceeds the limit.
-- For arrays indexed on @Int@, however, this is not going to happen in practice
-- since it requires more than 1024 PiB memory.
-- 
arrayLengthOverflowError :: a
arrayLengthOverflowError = error "Length of array has overflowed!"

-- | Utility Function. 
-- Takes the current length of an array and returns a larger length.
--
expandedSize :: Int -> Int
expandedSize = (1 +) . (`div` 2) . (3 *)

-- | Utility Function. 
-- Takes the needed length of an array and returns a larger
-- number as the physical length, so that some extra space is provided.
--
initialSize :: Int -> Int
initialSize = expandedSize . shiftL 1 . ceiling . logBase 2 . fromIntegral

-- | Utility Function. 
-- Returns an array out of bound error.
--
outOfBoundError :: Int -> a
outOfBoundError i
  = error $ "Index " ++ show i ++ " is out of bound!"


--------------------------------------------------------------------------------
-- Generic Binary Tree
--------------------------------------------------------------------------------


-- | Generic binary tree data type, mainly used for BST-based structures.
-- 
data GBTN e
  = GBEmpty
  | GBLeaf e
  | GBNode {-# UNPACK #-} !Int {-# UNPACK #-} !Int (GBTN e) e (GBTN e)
    deriving (Eq, Show)

instance Foldable GBTN where
  foldr f e tree = case tree of
    GBEmpty           -> e
    GBLeaf e'         -> f e' e
    GBNode _ _ l e' r -> foldr f (f e' (foldr f e r)) l

  toList = toList' []
    where
      toList' [] GBEmpty
        = []
      toList' ((GBNode _ _ _ e r) : stack) GBEmpty
        = e : toList' stack r
      toList'  [] (GBLeaf e)
        = [e]
      toList' ((GBNode _ _ _ e' r) : stack) (GBLeaf e)
        = e : e' : toList' stack r
      toList' stack bt@(GBNode _ _ l e' r)
        = toList' (bt : stack) l


-- | Adds an element to a binary search tree with a balancing function.
-- If the element exists already, the function replaces it and returns @False@.
-- Otherwise returns @True@.
-- The tree data type must be coercible with @GBT@.
-- 
addBT :: (Ord a, Coercible t GBTN) => (GBTN a -> GBTN a) -> a -> t a -> t a
addBT f e set
  = coerce $ snd $ addGBTN e tree
  where
    addSize          = bool 0 1
    tree             = coerce set
    (isNotIn, tree') = addGBTN e tree
    addGBTN e tree   = case tree of
      GBEmpty   -> (True, GBLeaf e)
      GBLeaf e' -> addLeaf e'
      _         -> addNode tree
    addLeaf e'
      | e < e'    = (True, GBNode 2 2 (GBLeaf e) e' GBEmpty)
      | e > e'    = (True, GBNode 2 2 GBEmpty e' (GBLeaf e))
      | otherwise = (False, GBLeaf e)
    addNode (GBNode s d l e' r)
      | e < e'    
        = let (b, subT) = addGBTN e l
              d' = 1 + max (depthBTN r) (depthBTN subT)
          in (b, f $ GBNode (s + addSize b) d' subT e' r)
      | e > e'    
        = let (b, subT) = addGBTN e r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in (b, f $ GBNode (s + addSize b) d' l e' subT)
      | otherwise = (False, GBNode s d l e r)

-- | Tests if the element is in the tree.
-- The tree data type must be coercible with @GBTN@.
-- 
containsBT :: (Ord a, Coercible t GBTN) => t a -> a -> Bool
containsBT set e = let tree = coerce set in case searchGBTN e tree of
  GBEmpty -> False
  _       -> True

-- | Returns the depth of the tree.
-- The tree node data type must be coercible with @GBTN@.
-- 
depthBTN :: forall t a. (Ord a, Coercible t GBTN) => t a -> Int
depthBTN tree = case coerce tree :: GBTN a of
  GBNode _ d _ _ _ -> d
  GBLeaf _         -> 1
  _                -> 0

-- | Reduce a @GBNode@ to @GBLeaf@ if both children are @GBEmpty@.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
normGBTN :: GBTN a -> GBTN a
normGBTN (GBNode _ _ GBEmpty e GBEmpty)
  = GBLeaf e
normGBTN tree 
  = tree

-- | Removes the given element from the tree with a balancing function.
-- Returns a tuple consisting of the removed element (or Nothing, if the
-- element is not in the tree), and the tree without the element.
-- The tree data type must be coercible with @GBT@.
--
removeBT :: (Ord a, Coercible t GBTN) 
         => (GBTN a -> GBTN a) 
         -> a 
         -> t a 
         -> (Maybe a, t a)
removeBT f e set
  = coerce $ removeGBTN tree
  where
    delSize     = bool 0 (-1) . isJust
    tree        = coerce set
    (me, tree') = removeGBTN tree
    removeGBTN tree = case tree of
      GBEmpty   -> (Nothing, tree)
      GBLeaf e' -> removeLeaf e'
      _         -> removeNode tree
    removeLeaf e'
      | e == e'   = (Just e', GBEmpty) 
      | otherwise = (Nothing, GBLeaf e') 
    removeNode (GBNode _ _ GBEmpty e' GBEmpty) 
      = removeGBTN (GBLeaf e')
    removeNode (GBNode s d l e' r)
      | e < e'       
        = let (me, subT) = removeGBTN l
              d' = 1 + max (depthBTN r) (depthBTN subT)
          in (me, f $ normGBTN $ GBNode (s + delSize me) d' subT e' r)
      | e > e' 
        = let (me, subT) = removeGBTN r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in (me, f $ normGBTN $ GBNode (s + delSize me) d' l e' subT)
      | GBEmpty <- l = (Just e', r)
      | GBEmpty <- r = (Just e', l)
      | otherwise    
        = let (Just eSucc, subT) = removeMinGBTN f r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in (Just e', f $ normGBTN $ GBNode (s - 1) d' l eSucc subT)

-- | Removing the minimum (most left) element from the tree with a balancing
-- function.
-- The tree data type must be coercible with @GBT@.
-- 
removeMinBT :: (Ord a, Coercible t GBTN) 
            => (GBTN a -> GBTN a) 
            -> t a 
            -> (Maybe a, t a)
removeMinBT f set
  = (me, coerce tree')
  where
    tree        = coerce set
    (me, tree') = removeMinGBTN f tree

-- | Removing the minimum (most left) element from the tree with a balancing
-- function.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
removeMinGBTN :: Ord a => (GBTN a -> GBTN a) -> GBTN a -> (Maybe a, GBTN a)
removeMinGBTN f (GBNode _ d GBEmpty e r)
  = (Just e, r)
removeMinGBTN f (GBNode s d l e r)
  = let (me, subT) = removeMinGBTN f l 
        d' = 1 + max (depthBTN r) (depthBTN subT)
    in (me, f $ normGBTN $ GBNode (s - if isJust me then 1 else 0) d' subT e r)
removeMinGBTN _ (GBLeaf e)
  = (Just e, GBEmpty)
removeMinGBTN _ _ 
  = (Nothing, GBEmpty)

-- | Returns the root of the tree.
-- The tree data type must be coercible with @GBT@.
-- 
rootBT :: (Ord a, Coercible t GBTN) => t a -> Maybe a
rootBT set = let tree = coerce set in case tree of
  GBEmpty          -> Nothing
  GBLeaf e         -> Just e
  GBNode _ _ _ e _ -> Just e

-- | Rotate the tree to the left with the given node as the root.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
rotateLeftGBTN :: Ord a => GBTN a -> GBTN a
rotateLeftGBTN (GBNode s _ l e (GBNode _ _ rl re rr))
  = GBNode s (1 + max (depthBTN ll) (depthBTN rr)) ll re rr
  where
    s' = 1 + sizeGBTN l + sizeGBTN rl
    ll = normGBTN $ GBNode s' (1 + max (depthBTN l) (depthBTN rl)) l e rl
rotateLeftGBTN (GBNode s _ l e (GBLeaf re))
  = let dl = depthBTN l 
    in GBNode s (1 + dl) (normGBTN $ GBNode (s - 1) dl l e GBEmpty) re GBEmpty
rotateLeftGBTN tree
  = tree

-- | Rotate the tree to the right with the given node as the root.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
rotateRightGBTN :: Ord a => GBTN a -> GBTN a
rotateRightGBTN (GBNode s _ (GBNode _ _ ll le lr) e r)
  = GBNode s (1 + max (depthBTN ll) (depthBTN rr)) ll le rr
  where
    s' = 1 + sizeGBTN r + sizeGBTN lr
    rr = normGBTN $ GBNode s' (1 + max (depthBTN r) (depthBTN lr)) lr e r
rotateRightGBTN (GBNode s _ (GBLeaf le) e r)
  = let dl = depthBTN r
    in GBNode s (1 + dl) GBEmpty le (normGBTN $ GBNode (s - 1) dl GBEmpty e r) 
rotateRightGBTN tree
  = tree

-- | Searches for the sub-tree with the given element as the root.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
searchGBTN :: Ord a => a -> GBTN a -> GBTN a
searchGBTN e
  = search'
  where
    search' GBEmpty
      = GBEmpty
    search' t@(GBLeaf e')
      = if e == e' then t else GBEmpty
    search' t@(GBNode _ _ l e' r)
      | e < e'    = search' l
      | e > e'    = search' r
      | otherwise = t

-- | Returns the size (number of elements) of the tree.
-- 
sizeGBTN :: GBTN a -> Int
sizeGBTN tree = case tree of
  GBNode s _ _ _ _ -> s
  GBLeaf _         -> 1
  _                -> 0
