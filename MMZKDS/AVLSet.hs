module MMZKDS.AVLSet where

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.Set (Set(..))
import           MMZKDS.Utilities 
  (GenericBinaryTree, addGenericBinaryTree, containsGenericBinaryTree,
   depthGenericBinaryTree
  )

-- | 
-- It is expected that the type of its elements is an instance of 'Ord'.
-- It has O(log n) adding, O(log n) deleting, O(log n) searching, O(n * log n) 
-- union and intersection, and O(n * log n) construction from list.
-- 
newtype AVLSet e = AVLSet (GenericBinaryTree e)


