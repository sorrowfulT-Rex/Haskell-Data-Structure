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
-- Minimal implementation requires @mAdd@, @mContains@, "mFindAny" and
-- @mRemove@.
-- Default methods include @mDifference@, @mDropAny@, @mIntersection@,
-- @mNewSet@, @mToList @and @mUnion@.
--
class (Monad (m s), MDS c m s, MDSCons [e] c m s) 
  => MSet c e m s | c -> e where
  -- | Adds an element into the set.
  -- If the element is not already in the set, adds it to the set, otherwise
  -- it replaces the old element in the set with the new one.
  --
  mAdd :: c s -> e -> m s ()

  -- | Tests if the element is in the set.
  --
  mContains :: c s -> e -> m s Bool

  -- | Returns an element from the set if it is non-empty.
  -- Does not guarantee which element is returned.
  -- 
  mFindAny :: c s -> m s (Maybe e)

  -- | Removes an element into the set.
  -- Returns the removed element (or Nothing, if element is not in the set), and 
  -- deletes the element from the set.
  --
  mRemove :: c s -> e -> m s (Maybe e)

  -- | Default method.
  -- Computes the difference of two sets, and update it to the first set.
  --
  mDifference :: forall c1. MDSCons [e] c1 m s => c s -> c1 s -> m s ()
  mDifference
    = flip ((>>=) . (MDS.finish :: c1 s -> m s [e])) . mapM_ . mRemove

  -- | Default method.
  -- Computes the difference of two sets, and update it to the first set.
  -- The second set is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  mDifference' :: c s -> c s -> m s ()
  mDifference' = mDifference

  mDropAny :: c s -> m s (Maybe e)
  mDropAny s = do
    me <- mFindAny s
    when (isJust me) $ void $ mRemove s (fromJust me)
    return me

  -- | Default method.
  -- Computes the intersection of two sets, and update it to the first set.
  --
  mIntersection :: forall c1. MDSCons [e] c1 m s => c s -> c1 s -> m s ()
  mIntersection d ds = do
    d' <- MDS.copy d
    mDifference d' ds
    mDifference d d'

  -- | Default method.
  -- Computes the intersection of two sets, and update it to the first set.
  -- The second set is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  mIntersection' :: c s -> c s -> m s ()
  mIntersection' = mIntersection

  -- | Default method.
  -- Returns a new set from @[]@.
  -- 
  mNewSet :: [e] -> m s (c s)
  mNewSet = MDS.new

  -- | Default method.
  -- Return the list representation of the set.
  -- 
  mToList :: c s -> m s [e]
  mToList = MDS.finish

  -- | Default method.
  -- Computes the union of two sets, and update it to the first set.
  --
  mUnion :: forall c1. MDSCons [e] c1 m s => c s -> c1 s -> m s ()
  mUnion = flip ((>>=) . (MDS.finish :: c1 s -> m s [e])) . mapM_ . mAdd

  -- | Default method.
  -- Computes the union of two sets, and update it to the first set.
  -- The second set is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  mUnion' :: c s -> c s -> m s ()
  mUnion' = mUnion
