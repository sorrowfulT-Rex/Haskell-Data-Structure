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
data GenericBinaryTree e
  = GBEmpty
  | GBLeaf e
  | GBNode {-# UNPACK #-} !Int (GenericBinaryTree e) e (GenericBinaryTree e)
  deriving (Eq, Show)

instance {-# OVERLAPPABLE #-} (Coercible t GenericBinaryTree)
  => Foldable t where
  foldr f e tree = case (coerce :: t e -> GenericBinaryTree e) tree of
    GBEmpty         -> e
    GBLeaf e'       -> f e' e
    GBNode _ l e' r -> foldr f (f e' (foldr f e r)) l

  toList = toList' [] . (coerce :: t e -> GenericBinaryTree e)
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


-- | Adds an element to a binary search tree without self-balancing.
-- If the element exists already, the function replaces it and returns @False@.
-- Otherwise returns @True@.
-- The tree data type must be coercible with @GenericBinaryTree@.
-- 
addGenericBinaryTree :: forall t e. (Ord e, Coercible t GenericBinaryTree)
                     => e
                     -> t e
                     -> (Bool, t e)
addGenericBinaryTree e tree = case gTree of
  GBEmpty   -> (True, coerce $ GBLeaf e)
  GBLeaf e' -> addLeaf e'
  _         -> addNode gTree
  where
    gTree = coerce tree
    addLeaf e'
      | e < e'    = (True, coerce $ GBNode 1 (GBLeaf e) e' GBEmpty)
      | e > e'    = (True, coerce $ GBNode 1 GBEmpty e' (GBLeaf e))
      | otherwise = (False, coerce $ GBLeaf e)
    addNode (GBNode d l e' r)
      | e < e'    = let (b, subT) = addGenericBinaryTree e l
        in (b, coerce $
          GBNode (max d $ 1 + depthGenericBinaryTree subT) subT e' r)
      | e > e'    = let (b, subT) = addGenericBinaryTree e r
        in (b, coerce $
          GBNode (max d $ 1 + depthGenericBinaryTree subT) l e' subT)
      | otherwise = (False, coerce $ GBNode d l e r)

-- | Tests if the element is in the tree.
-- The tree data type must be coercible with @GenericBinaryTree@.
-- 
containsGenericBinaryTree :: forall t e. (Ord e, Coercible t GenericBinaryTree)
                          => t e
                          -> e
                          -> Bool
containsGenericBinaryTree t e = case searchGenericBinaryTree e t of
  GBEmpty -> False
  _       -> True

-- | Returns the depth of the tree.
-- The tree data type must be coercible with @GenericBinaryTree@.
-- 
depthGenericBinaryTree :: forall t e. (Ord e, Coercible t GenericBinaryTree)
                       => t e
                       -> Int
depthGenericBinaryTree tree = case coerce tree :: GenericBinaryTree e of
  GBNode d _ _ _ -> d
  _              -> 0

-- | Removes the given element from the tree.
-- Returns a tuple consisting of the removed element (or Nothing, if the
-- element is not in the tree), and the tree without the element.
-- The tree data type must be coercible with @GenericBinaryTree@.
--
removeGenericBinaryTree :: forall t e. (Ord e, Coercible t GenericBinaryTree)
                        => e
                        -> t e
                        -> (Maybe e, t e)
removeGenericBinaryTree e tree = case gTree of
  GBEmpty   -> (Nothing, tree)
  GBLeaf e' -> removeLeaf e'
  _         -> removeNode gTree
  where
    gTree = coerce tree
    removeLeaf e'
      | e == e'   = (Just e', coerce (GBEmpty :: GenericBinaryTree e)) 
      | otherwise = (Nothing, tree) 
    removeNode (GBNode d l e' r)
      | e < e'       = let (me, subT) = removeGenericBinaryTree e l
        in (me, coerce $
          GBNode (max d $ 1 + depthGenericBinaryTree subT) subT e' r)
      | e > e'       = let (me, subT) = removeGenericBinaryTree e r
        in (me, coerce $
          GBNode (max d $ 1 + depthGenericBinaryTree subT) l e' subT)
      | GBEmpty <- l = (Just e', coerce r)
      | GBEmpty <- r = (Just e', coerce l)

-- | Returns the root of the tree.
-- The tree data type must be coercible with @GenericBinaryTree@.
-- 
rootGenericBinaryTree :: forall t e. (Ord e, Coercible t GenericBinaryTree)
                      => t e
                      -> Maybe e
rootGenericBinaryTree tree = case coerce tree :: GenericBinaryTree e of
  GBEmpty        -> Nothing
  GBLeaf e       -> Just e
  GBNode _ _ e _ -> Just e

-- | Searches for the sub-tree with the given element as the root.
-- The tree data type must be coercible with @GenericBinaryTree@.
-- 
searchGenericBinaryTree :: forall t e. (Ord e, Coercible t GenericBinaryTree)
                        => e
                        -> t e
                        -> GenericBinaryTree e
searchGenericBinaryTree e tree
  = search' (coerce tree)
  where
    search' GBEmpty
      = GBEmpty
    search' t@(GBLeaf e')
      = if e == e' then t else GBEmpty
    search' t@(GBNode _ l e' r)
      | e < e'    = search' l
      | e > e'    = search' r
      | otherwise = t
