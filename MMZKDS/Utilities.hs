{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.Utilities where

import           Data.Bits (shiftL)
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
data GBT e 
  = GBT
    {-# UNPACK #-} !Int  -- ^ Size of the tree
    (GBTN e)  -- ^ Root of the tree
    deriving Eq

instance Show a => Show (GBT a) where
  show = ("Set: " ++) . show . toList

-- | Node for @GBT@.
-- 
data GBTN e
  = GBEmpty
  | GBLeaf e
  | GBNode {-# UNPACK #-} !Int (GBTN e) e (GBTN e)
    deriving (Eq, Show)

instance {-# OVERLAPPABLE #-} Coercible t GBT => Foldable t where
  foldr f e set
    = let GBT _ tree = (coerce :: t a -> GBT a) set in foldr f e tree

  length set
    = let GBT n _ = (coerce :: t a -> GBT a) set in n

  toList set 
    = let GBT _ tree = (coerce :: t a -> GBT a) set in toList tree

instance Foldable GBTN where
  foldr f e tree = case tree of
    GBEmpty         -> e
    GBLeaf e'       -> f e' e
    GBNode _ l e' r -> foldr f (f e' (foldr f e r)) l

  toList = toList' []
    where
      toList' [] GBEmpty
        = []
      toList' ((GBNode _ _ e r) : stack) GBEmpty
        = e : toList' stack r
      toList'  [] (GBLeaf e)
        = [e]
      toList' ((GBNode _ _ e' r) : stack) (GBLeaf e)
        = e : e' : toList' stack r
      toList' stack bt@(GBNode _ l e' r)
        = toList' (bt : stack) l


-- | Adds an element to a binary search tree with a balancing function.
-- If the element exists already, the function replaces it and returns @False@.
-- Otherwise returns @True@.
-- The tree data type must be coercible with @GBT@.
-- 
addBT :: (Ord a, Coercible t GBT) => (GBTN a -> GBTN a) -> a -> t a -> t a
addBT f e set
  | isNotIn   = coerce $ GBT (n + 1) tree'
  | otherwise = set
  where
    GBT n tree       = coerce set
    (isNotIn, tree') = addGBTN e tree
    addGBTN e tree   = case tree of
      GBEmpty   -> (True, GBLeaf e)
      GBLeaf e' -> addLeaf e'
      _         -> addNode tree
    addLeaf e'
      | e < e'    = (True, GBNode 2 (GBLeaf e) e' GBEmpty)
      | e > e'    = (True, GBNode 2 GBEmpty e' (GBLeaf e))
      | otherwise = (False, GBLeaf e)
    addNode (GBNode d l e' r)
      | e < e'    = let (b, subT) = addGBTN e l
        in (b, f $ GBNode (1 + max (depthBTN r) (depthBTN subT)) subT e' r)
      | e > e'    = let (b, subT) = addGBTN e r
        in (b, f $ GBNode (1 + max (depthBTN l) (depthBTN subT)) l e' subT)
      | otherwise = (False, GBNode d l e r)

-- | Tests if the element is in the tree.
-- The tree data type must be coercible with @GBTN@.
-- 
containsBT :: forall t e. (Ord e, Coercible t GBT) => t e -> e -> Bool
containsBT set e = let GBT _ tree = coerce set in case searchGBTN e tree of
  GBEmpty -> False
  _       -> True

-- | Returns the depth of the tree.
-- The tree node data type must be coercible with @GBTN@.
-- 
depthBTN :: forall t a. (Ord a, Coercible t GBTN) => t a -> Int
depthBTN tree = case coerce tree :: GBTN a of
  GBNode d _ _ _ -> d
  GBLeaf _       -> 1
  _              -> 0

-- | Returns an empty @GBT@.
-- 
emptyGBT :: GBT a
emptyGBT = GBT 0 GBEmpty

-- | Reduce a @GBNode@ to @GBLeaf@ if both children are @GBEmpty@.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
normGBTN :: GBTN a -> GBTN a
normGBTN (GBNode _ GBEmpty e GBEmpty)
  = GBLeaf e
normGBTN tree 
  = tree

-- | Removes the given element from the tree with a balancing function.
-- Returns a tuple consisting of the removed element (or Nothing, if the
-- element is not in the tree), and the tree without the element.
-- The tree data type must be coercible with @GBT@.
--
removeBT :: forall t a. (Ord a, Coercible t GBT) 
         => (GBTN a -> GBTN a) 
         -> a -> t a -> (Maybe a, t a)
removeBT f e set
  | isJust me = (me, coerce $ GBT (n - 1) tree')
  | otherwise = (me, set)
  where
    GBT n tree  = coerce set
    (me, tree') = removeGBTN e tree
    removeGBTN e tree = case tree of
      GBEmpty   -> (Nothing, tree)
      GBLeaf e' -> removeLeaf e'
      _         -> removeNode tree
    removeLeaf e'
      | e == e'   = (Just e', GBEmpty) 
      | otherwise = (Nothing, tree) 
    removeNode (GBNode d GBEmpty e' GBEmpty) 
      = removeGBTN e (GBLeaf e')
    removeNode (GBNode d l e' r)
      | e < e'       
        = let (me, subT) = removeGBTN e l
              d' = 1 + max (depthBTN r) (depthBTN subT)
          in (me, f $ normGBTN $ GBNode d' subT e' r)
      | e > e' 
        = let (me, subT) = removeGBTN e r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in (me, f $ normGBTN $ GBNode d' l e' subT)
      | GBEmpty <- l = (Just e', r)
      | GBEmpty <- r = (Just e', l)
      | otherwise    
        = let (Just eSucc, subT) = removeMinGBTN f r
              d' = 1 + max (depthBTN l) (depthBTN subT)
          in (Just e', f $ normGBTN $ GBNode d' l eSucc subT)

-- | Removing the minimum (most left) element from the tree with a balancing
-- function.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
removeMinGBTN :: Ord a => (GBTN a -> GBTN a) -> GBTN a -> (Maybe a, GBTN a)
removeMinGBTN f (GBNode d GBEmpty e r)
  = (Just e, r)
removeMinGBTN f (GBNode d l e r)
  = let (me, subT) = removeMinGBTN f l 
        d' = 1 + max (depthBTN r) (depthBTN subT)
    in (me, f $ normGBTN $ GBNode d' subT e r)
removeMinGBTN _ (GBLeaf e)
  = (Just e, GBEmpty)
removeMinGBTN _ _ 
  = (Nothing, GBEmpty)

-- | Returns the root of the tree.
-- The tree data type must be coercible with @GBT@.
-- 
rootBT :: forall t a. (Ord a, Coercible t GBT) => t a -> Maybe a
rootBT set = let GBT _ tree = coerce set in case tree of
  GBEmpty        -> Nothing
  GBLeaf e       -> Just e
  GBNode _ _ e _ -> Just e

-- | Rotate the tree to the left with the given node as the root.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
rotateLeftGBTN :: Ord a => GBTN a -> GBTN a
rotateLeftGBTN (GBNode _ l e (GBNode d rl re rr))
  = GBNode (1 + max (depthBTN ll) (depthBTN rr)) ll re rr
  where
    ll = normGBTN $ GBNode (1 + max (depthBTN l) (depthBTN rl)) l e rl
rotateLeftGBTN (GBNode _ l e (GBLeaf re))
  = let dl = depthBTN l 
    in GBNode (1 + dl) (normGBTN $ GBNode dl l e GBEmpty) re GBEmpty
rotateLeftGBTN tree
  = tree

-- | Rotate the tree to the right with the given node as the root.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
rotateRightGBTN :: Ord a => GBTN a -> GBTN a
rotateRightGBTN (GBNode _ (GBNode d ll le lr) e r)
  = GBNode (1 + max (depthBTN ll) (depthBTN rr)) ll le rr
  where
    rr = normGBTN $ GBNode (1 + max (depthBTN r) (depthBTN lr)) lr e r
rotateRightGBTN (GBNode _ (GBLeaf le) e r)
  = let dl = depthBTN r
    in GBNode (1 + dl) GBEmpty le (normGBTN $ GBNode dl GBEmpty e r) 
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
    search' t@(GBNode _ l e' r)
      | e < e'    = search' l
      | e > e'    = search' r
      | otherwise = t
