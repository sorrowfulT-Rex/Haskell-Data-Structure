{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.Set where

import           Control.Monad (liftM2)
import           Data.List (foldl')

import           MMZKDS.DS as DS (DS(..), DSCons(..))
import           MMZKDS.MDS as MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- Set Type Class
--------------------------------------------------------------------------------

-- | 'Set' is a type class for immutable set data structures where the elements
-- are unique, with methods including addition, deletion, union, intersection
-- and so on.
-- It is expected that the elements are instances of 'Eq'.  
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @add@, @contains@ and @remove@.
-- Default methods include @difference@, @intersection@, @newSet@, @toList@
-- and @union@.
-- For functional operations, one can either create a 'Monad' instance, or
-- "stream" the list structure with @toList@, apply the functions, then 
-- "collect" it back with "@newList@".
--
class (DS (c e), DSCons [e] (c e)) => Set c e where
  -- | Adds an element into the set.
  -- If the element is not already in the set, returns a new set with this
  -- element, otherwise returns the same set.
  --
  add :: e -> c e -> c e

  -- | Tests if the element is in the set.
  --
  contains :: e -> c e -> Bool

  -- | Removes an element into the set.
  -- Returns a tuple consisting of the removed element (or Nothing, if the
  -- element is not in the set), and the set without the element.
  --
  remove :: e -> c e -> (Maybe e, c e)

  -- | Default method.
  -- Computes the difference of two sets.
  --
  difference :: forall c1. DSCons [e] (c1 e) => c e -> c1 e -> c e
  difference = (. (DS.finish :: c1 e -> [e])) . foldl' ((snd .) . flip remove)

  -- | Default method.
  -- Computes the intersection of two sets.
  --
  intersection :: forall c1. DSCons [e] (c1 e) => c e -> c1 e -> c e
  intersection = liftM2 (.) difference difference

  -- | Default method.
  -- Returns a new set from @[]@.
  newSet :: [e] -> c e
  newSet = DS.new

  -- | Default method.
  -- Return the list representation of the set.
  toList :: c e -> [e]
  toList = DS.finish

  -- | Default method.
  -- Computes the union of two sets.
  --
  union :: forall c1. DSCons [e] (c1 e) => c e -> c1 e -> c e
  union = (. (DS.finish :: c1 e -> [e])) . foldl' (flip add)

-- | 'MSet' is a type class for mutable set structures based on the @ST@-monad
-- where the elements are unique, with methods including addition, deletion,
-- union, intersection and so on.
-- It is expected that the elements are instances of 'Eq'.  
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @mAdd@, @mContains@ and @mRemove@.
-- Default methods include @mDifference@, @mIntersection@, @mNewSet@, @mToList@
-- and @mUnion@.
--
class (Monad (m s), MDS (c e) m s, MDSCons [e] (c e) m s) => MSet c e m s where
  -- | Adds an element into the set.
  -- If the element is not already in the set, adds it to the set, otherwise
  -- it does nothing.
  --
  mAdd :: e -> c e s -> m s ()

  -- | Tests if the element is in the set.
  --
  mContains :: e -> c e s -> m s Bool

  -- | Removes an element into the set.
  -- Returns the removed element (or Nothing, if element is not in the set), and 
  -- deletes the element from the set.
  --
  mRemove :: e -> c e s -> m s (Maybe e)

  -- | Default method.
  -- Computes the difference of two sets.
  --
  mDifference :: forall c1. MDSCons [e] (c1 e) m s
              => c e s
              -> c1 e s
              -> m s ()
  mDifference
    = flip ((>>=) . (MDS.finish :: c1 e s -> m s [e])) . mapM_ . flip mRemove

  -- | Default method.
  -- Computes the intersection of two sets.
  --
  mIntersection :: forall c1. MDSCons [e] (c1 e) m s
                => c e s
                -> c1 e s
                -> m s ()
  mIntersection d ds = do
    d' <- MDS.copy d
    mDifference d' ds
    mDifference d d'

  -- | Default method.
  -- Returns a new set from @[]@.
  mNewSet :: [e] -> m s (c e s)
  mNewSet = MDS.new

  -- | Default method.
  -- Return the list representation of the set.
  mToList :: c e s -> m s [e]
  mToList = MDS.finish

  -- | Default method.
  -- Computes the union of two sets.
  --
  mUnion :: forall c1. MDSCons [e] (c1 e) m s
         => c e s
         -> c1 e s
         -> m s ()
  mUnion = flip ((>>=) . (MDS.finish :: c1 e s -> m s [e])) . mapM_ . flip mAdd
