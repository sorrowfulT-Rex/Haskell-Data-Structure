{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.Class.MSet where

import           Control.Monad (void, when)
import           Data.Maybe (isJust, fromJust)

import           MMZKDS.Class.MDS as MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- MSet Type Class
--------------------------------------------------------------------------------

-- | 'MSet' is a type class for mutable set structures based on the @ST@-monad
-- where the elements are unique, with methods including addition, deletion,
-- union, intersection and so on.
-- It is expected that the elements are instances of 'Ord'.  
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @add@, @contains@, "findAny" and
-- @remove@.
-- Default methods include @difference@, @difference'@, @dropAny@, 
-- @intersection@, @intersection'@, @newSet@, @toList, @union@ and 
-- @union'@.
--
class (Monad (m s), MDS c m s, MDSCons [e] c m s) 
  => MSet c e m s | c -> e where
  -- | Adds an element into the set.
  -- If the element is not already in the set, adds it to the set, otherwise
  -- it replaces the old element in the set with the new one.
  --
  add :: c s -> e -> m s ()

  -- | Tests if the element is in the set.
  --
  contains :: c s -> e -> m s Bool

  -- | Returns an element from the set if it is non-empty.
  -- Does not guarantee which element is returned.
  -- 
  findAny :: c s -> m s (Maybe e)

  -- | Removes an element into the set.
  -- Returns the removed element (or Nothing, if element is not in the set), and 
  -- deletes the element from the set.
  --
  remove :: c s -> e -> m s (Maybe e)

  -- | Default method.
  -- Computes the difference of two sets, and update it to the first set.
  --
  difference :: forall c1. MDSCons [e] c1 m s => c s -> c1 s -> m s ()
  difference
    = flip ((>>=) . (MDS.finish :: c1 s -> m s [e])) . mapM_ . remove

  -- | Default method.
  -- Computes the difference of two sets, and update it to the first set.
  -- The second set is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  difference' :: c s -> c s -> m s ()
  difference' = difference

  dropAny :: c s -> m s (Maybe e)
  dropAny s = do
    me <- findAny s
    when (isJust me) $ void $ remove s (fromJust me)
    return me

  -- | Default method.
  -- Computes the intersection of two sets, and update it to the first set.
  --
  intersection :: forall c1. MDSCons [e] c1 m s => c s -> c1 s -> m s ()
  intersection d ds = do
    d' <- MDS.copy d
    difference d' ds
    difference d d'

  -- | Default method.
  -- Computes the intersection of two sets, and update it to the first set.
  -- The second set is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  intersection' :: c s -> c s -> m s ()
  intersection' = intersection

  -- | Default method.
  -- Returns a new set from @[]@.
  -- 
  newSet :: [e] -> m s (c s)
  newSet = MDS.new

  -- | Default method.
  -- Return the list representation of the set.
  -- 
  toList :: c s -> m s [e]
  toList = MDS.finish

  -- | Default method.
  -- Computes the union of two sets, and update it to the first set.
  --
  union :: forall c1. MDSCons [e] c1 m s => c s -> c1 s -> m s ()
  union = flip ((>>=) . (MDS.finish :: c1 s -> m s [e])) . mapM_ . add

  -- | Default method.
  -- Computes the union of two sets, and update it to the first set.
  -- The second set is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  union' :: c s -> c s -> m s ()
  union' = union
