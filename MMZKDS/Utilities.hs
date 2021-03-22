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


-- | Adds an element to a binary search tree without self-balancing.
-- If the element exists already, the function replaces it and returns @False@.
-- Otherwise returns @True@.
-- The tree data type must be coercible with @GBT@.
-- 
addBT :: (Ord a, Coercible t GBT) 
      => a 
      -> t a 
      -> t a
addBT e set
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
      | e < e'    = (True, GBNode 1 (GBLeaf e) e' GBEmpty)
      | e > e'    = (True, GBNode 1 GBEmpty e' (GBLeaf e))
      | otherwise = (False, GBLeaf e)
    addNode (GBNode d l e' r)
      | e < e'    = let (b, subT) = addGBTN e l
        in (b, GBNode (max d $ 1 + depthGBTN subT) subT e' r)
      | e > e'    = let (b, subT) = addGBTN e r
        in (b, GBNode (max d $ 1 + depthGBTN subT) l e' subT)
      | otherwise = (False, GBNode d l e r)

-- | Tests if the element is in the tree.
-- The tree data type must be coercible with @GBTN@.
-- 
containsBT :: forall t e. (Ord e, Coercible t GBT)
           => t e
           -> e
           -> Bool
containsBT set e = let GBT _ tree = coerce set in case searchGBTN e tree of
  GBEmpty -> False
  _       -> True

-- | Returns the depth of the tree.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
depthGBTN :: GBTN e -> Int
depthGBTN tree = case tree of
  GBNode d _ _ _ -> d
  _              -> 0

-- | Returns an empty @GBT@.
-- 
emptyGBT :: GBT e
emptyGBT = GBT 0 GBEmpty

-- | Removes the given element from the tree.
-- Returns a tuple consisting of the removed element (or Nothing, if the
-- element is not in the tree), and the tree without the element.
-- The tree data type must be coercible with @GBTN@.
--
removeBT :: forall t e. (Ord e, Coercible t GBT)
         => e
         -> t e
         -> (Maybe e, t e)
removeBT e set
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
    removeNode (GBNode d l e' r)
      | e < e'       = let (me, subT) = removeGBTN e l
        in (me, GBNode (max d $ 1 + depthGBTN subT) subT e' r)
      | e > e'       = let (me, subT) = removeGBTN e r
        in (me, GBNode (max d $ 1 + depthGBTN subT) l e' subT)
      | GBEmpty <- l = (Just e', r)
      | GBEmpty <- r = (Just e', l)

-- | Returns the root of the tree.
-- The tree data type must be coercible with @GBTN@.
-- 
rootBT :: forall t e. (Ord e, Coercible t GBT)
       => t e
       -> Maybe e
rootBT set = let GBT _ tree = coerce set in case tree of
  GBEmpty        -> Nothing
  GBLeaf e       -> Just e
  GBNode _ _ e _ -> Just e

-- | Searches for the sub-tree with the given element as the root.
-- Works for the Generic Binary Tree Node data type @GBTN@.
-- 
searchGBTN :: Ord e => e -> GBTN e -> GBTN e
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
