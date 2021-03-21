module MMZKDS.Utilities where

import           Data.Bits (shiftL)
import           Data.Foldable (toList)

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

instance Foldable GenericBinaryTree where
  foldr _ e GBEmpty 
    = e
  foldr f e (GBLeaf e') 
    = f e' e
  foldr f e (GBNode _ l e' r)
    = foldr f (f e' (foldr f e r)) l

  toList
    = toList' []
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
-- If the element exists already, replace it.
-- 
addGenericBinaryTree :: Ord e => e -> GenericBinaryTree e -> GenericBinaryTree e
addGenericBinaryTree e GBEmpty
  = GBLeaf e
addGenericBinaryTree e (GBLeaf e')
  | e < e'    = GBNode 1 (GBLeaf e) e' GBEmpty
  | e > e'    = GBNode 1 GBEmpty e' (GBLeaf e)
  | otherwise = GBLeaf e
addGenericBinaryTree e (GBNode d l e' r)
  | e < e'    = let subT = addGenericBinaryTree e l 
    in GBNode (max d $ 1 + depthGenericBinaryTree subT) subT e' r
  | e > e'    = let subT = addGenericBinaryTree e r 
    in GBNode (max d $ 1 + depthGenericBinaryTree subT) l e' subT
  | otherwise = GBNode d l e r

-- | Returns the depth of the tree.
-- 
depthGenericBinaryTree :: GenericBinaryTree e -> Int
depthGenericBinaryTree (GBNode d _ _ _)
  = d
depthGenericBinaryTree _
  = 0
