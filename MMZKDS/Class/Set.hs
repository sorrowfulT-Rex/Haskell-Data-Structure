{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Class.Set (Set(..)) where

import           Control.Monad (liftM2)
import           Data.List (foldl')

import           MMZKDS.Class.DS as DS (DS(..), DSCons(..))


--------------------------------------------------------------------------------
-- Set Type Class
--------------------------------------------------------------------------------

-- | 'Set' is a type class for immutable set data structures where the elements
-- are unique, with methods including addition, deletion, union, intersection
-- and so on.
-- It is expected that the elements are instances of 'Ord'.  
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @add@, @contains@, @findAny@, and @remove@.
-- Default methods include @difference@, @difference'@, @dropAny@, 
-- @intersection@, @intersection'@, @newSet@, @toList@, @union@ and @union'@.
-- For functional operations, one can either create a 'Monad' instance, or
-- "stream" the set with @toList@, apply the functions, then "collect" it back 
-- with "@newSet@".
--
class (DS c, DSCons [e] c) => Set c e | c -> e where
  -- | Adds an element into the set.
  -- If the element is not already in the set, returns a new set with this
  -- element, otherwise replaces the old element in the set with the new one.
  --
  add :: c -> e -> c

  -- | Tests if the element is in the set.
  --
  contains :: c -> e -> Bool

  -- | Returns an element from the set if it is non-empty.
  -- Does not guarantee which element is returned.
  -- 
  findAny :: c -> Maybe e

  -- | Removes the given element from the set.
  -- Returns a tuple consisting of the removed element (or Nothing, if the
  -- element is not in the set), and the set without the element.
  --
  remove :: c -> e -> (Maybe e, c)

  -- | Default method.
  -- Computes the difference of two sets.
  --
  difference :: forall c1. DSCons [e] c1 => c -> c1 -> c
  difference = (. (DS.finish :: c1 -> [e])) . foldl' ((snd .) . remove)

  -- | Default method.
  -- Computes the difference of two sets, where the second set is of the same
  -- type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  difference' ::  c -> c -> c
  difference' = difference

  -- | Default method.
  -- Removes an element from the set.
  -- Returns a tuple consisting of the removed element (or Nothing, if the
  -- element is not in the set), and the set without the element.
  -- Does not guarantee which element is removed.
  -- 
  dropAny :: c -> (Maybe e, c)
  dropAny s 
    = maybe (Nothing, s) (remove s) (findAny s)

  -- | Default method.
  -- Computes the intersection of two sets.
  --
  intersection :: forall c1. DSCons [e] c1 => c -> c1 -> c
  intersection = liftM2 (.) difference difference

  -- | Default method.
  -- Computes the intersection of two sets, where the second set is of the same
  -- type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  intersection' :: c -> c -> c
  intersection' = intersection

  -- | Default method.
  -- Returns a new set from @[]@.
  -- 
  newSet :: [e] -> c
  newSet = DS.new

  -- | Default method.
  -- Return the list representation of the set.
  -- 
  toList :: c -> [e]
  toList = DS.finish

  -- | Default method.
  -- Computes the union of two sets.
  --
  union :: forall c1. DSCons [e] c1 => c -> c1 -> c
  union = (. (DS.finish :: c1 -> [e])) . foldl' add

  -- | Default method.
  -- Computes the union of two sets, where the second set is of the same
  -- type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  union' :: c -> c -> c
  union' = union


--------------------------------------------------------------------------------
-- Set -> Monoid
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (Set c e) => Semigroup c where
  (<>) = mappend

instance {-# OVERLAPPABLE #-} (Set c e) => Monoid c where
  mempty  = newSet []
  mappend = union
