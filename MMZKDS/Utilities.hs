module MMZKDS.Utilities where

import           Control.Monad.ST (ST)
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
-- Binary Tree
--------------------------------------------------------------------------------

-- | Generic data type for binary tree.
-- 
data BinaryTree e = BEmpty | BLeaf e | BNode (BinaryTree e) e (BinaryTree e)
  deriving (Eq, Show)

-- | Add an element to a binary search tree without self-balancing.
-- 
addBinaryTree :: Ord e => e -> BinaryTree e -> BinaryTree e
addBinaryTree e BEmpty
  = BLeaf e
addBinaryTree e (BLeaf e')
  | e < e'    = BNode (BLeaf e) e' BEmpty
  | e > e'    = BNode BEmpty e' (BLeaf e)
  | otherwise = BLeaf e
addBinaryTree e (BNode l e' r)
  | e < e'    = BNode (addBinaryTree e l) e' r
  | e > e'    = BNode l e' (addBinaryTree e r)
  | otherwise = BNode l e r

instance Foldable BinaryTree where
  foldr _ e BEmpty 
    = e
  foldr f e (BLeaf e') 
    = f e' e
  foldr f e (BNode l e' r)
    = foldr f (f e' (foldr f e r)) l

  toList bt
    = toList' bt []
    where
      toList' BEmpty []
        = []
      toList' BEmpty ((BNode _ e r) : stack)
        = e : toList' r stack
      toList' (BLeaf e) []
        = [e]
      toList' (BLeaf e) ((BNode _ e' r) : stack)
        = e : e' : toList' r stack
      toList' bt@(BNode l e' r) stack
        = toList' l $ bt : stack
