{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.RBTSet (RBTSet) where

import           Data.Coerce (coerce)
import           Data.Foldable (toList)
import           Data.List (foldl')
import           Data.Maybe (isJust)

import           MMZKDS.Base (RBColour(..), RBTSet(..))
import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.PriorityQueue (PriorityQueue(..))
import           MMZKDS.Set as S (Set(add, contains, findAny, remove))
import           MMZKDS.Utilities
  (GBTN(..), addBT, containsBT, depthBTN, rootBT, removeBT, removeMinBT, 
  rotateLeftGBTN, rotateRightGBTN
  )

instance (Ord a, Show a) => Show (RBTSet a) where
  show = ("Set: " ++) . show . (finish :: RBTSet a -> [a])


--------------------------------------------------------------------------------
-- Set Instance
--------------------------------------------------------------------------------

instance Ord a => Set RBTSet a where
  add :: a -> RBTSet a -> RBTSet a
  add e (RBTSet tree) 
    = RBTSet $ addBT balanceRBT (Red e) tree

  contains :: RBTSet a -> a -> Bool
  contains (RBTSet tree)
    = containsBT tree . Red
  
  findAny :: RBTSet a -> Maybe a
  findAny (RBTSet tree) 
    = getElem <$> rootBT tree

  remove :: a -> RBTSet a -> (Maybe a, RBTSet a)
  remove e (RBTSet tree)
    = let (me, tree) = removeBT balanceRBT (Red e) tree 
      in (getElem <$> me, RBTSet tree)


--------------------------------------------------------------------------------
-- PriorityQueue Instance
--------------------------------------------------------------------------------

instance Ord a => PriorityQueue RBTSet a where
  add :: a -> RBTSet a -> RBTSet a
  add = S.add

  -- pop :: RBTSet a -> (Maybe a, RBTSet a)
  -- pop = removeMinBT balanceRBT


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance DS (RBTSet a) where
--   clear :: RBTSet a -> RBTSet a
--   clear = const $ coerce (GBEmpty :: GBTN a)

--   size :: RBTSet a -> Int
--   size = length . (coerce :: RBTSet a -> GBTN a)

instance Ord a => DSCons [a] (RBTSet a) where
--   finish :: RBTSet a -> [a]
--   finish = toList . (coerce :: RBTSet a -> GBTN a)

--   new :: [a] -> RBTSet a
--   new = foldl' (flip S.add) $ coerce (GBEmpty :: GBTN a)


--------------------------------------------------------------------------------
-- Red-Black Tree Specific Function
--------------------------------------------------------------------------------

instance Eq a => Eq (RBColour a) where
  a == a'
    = getElem a == getElem a'

instance Ord a => Ord (RBColour a) where
  a <= a' 
   = getElem a <= getElem a'

-- | Utility Function.
-- A balancing function that re-balances the tree. It will be called by @add@
-- and @remove@.
-- 
balanceRBT :: Ord a => GBTN a -> GBTN a
balanceRBT = undefined


-- | Utility Function.
-- Retrieve the element from a coloured node.  
-- 
getElem :: RBColour a -> a
getElem (Red e)
  = e
getElem (Black e)
  = e
