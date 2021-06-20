{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Class.UnorderedList (UnorderedList(..)) where

import           Control.Monad (liftM2)
import           Data.List (foldl')

import           MMZKDS.Class.DS as DS (DS(..), DSCons(..))


--------------------------------------------------------------------------------
-- UnorderedList Type Class
--------------------------------------------------------------------------------

-- | 'UnorderedList' is a type class for immutable list structures where the 
-- elements are not ordered by index, but by its intrinsic order.
-- It is similar to a set that can have duplicates.
-- It is expected that the elements are instances of 'Ord'.  
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @add@, @contains@, @getNthMin@, and @remove@.
-- Default methods include @count@, @difference@, @difference'@, @dropAny@, 
-- @findAny@, @getMax@, @getNthMax@, @getMin@, @intersection@, @intersection'@, 
-- @newUnorderedList@, @toList@, @union@ and @union'@.
--
class (DS c, DSCons [e] c) => UnorderedList c e | c -> e where
  -- | Adds an element into the list.
  add :: c -> e -> c

  -- | Tests if the element is in the set.
  -- Returns the range of position of this element in the list. If it does not
  -- exist, returns Nothing.
  --
  contains :: c -> e -> Maybe (Int, Int)

  -- | Gets the n-th minimum element in the list.
  --
  getNthMin :: c -> Int -> Maybe e

  -- | Removes the given element from the set.
  -- Returns a tuple consisting of the removed element (or Nothing, if the
  -- element is not in the set), and the set without the element.
  --
  remove :: c -> e -> (Maybe e, c)

  -- | Default method.
  -- Counts the number of occurrences of an element in the list.
  --
  count :: c -> e -> Int

  -- | Default method.
  -- Computes the difference of two sets.
  --
  difference :: forall c1. DSCons [e] c1 => c -> c1 -> c
  difference = (. (DS.finish :: c1 -> [e])) . foldl' ((snd .) . remove)

  -- | Default method.
  -- Computes the difference of two sets, where the second set is of the same
  -- type as the first one.
  -- For some instances of "UnorderedList" this may have a more efficient 
  -- implementation.
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
  -- Returns an element from the list if it is non-empty.
  -- Does not guarantee which element is returned.
  -- 
  findAny :: c -> Maybe e
  findAny = flip getNthMin 1

  -- | Gets the maximum element in the list.
  --
  getMax :: c -> Maybe e
  getMax = flip getNthMax 1

  -- | Gets the maximum element in the list.
  --
  getNthMax :: c -> Int -> Maybe e
  getNthMax c i = getNthMin c (size c - i + 1)

  -- | Default method.
  -- Computes the intersection of two sets.
  --
  intersection :: forall c1. DSCons [e] c1 => c -> c1 -> c
  intersection = liftM2 (.) difference difference

  -- | Default method.
  -- Computes the intersection of two sets, where the second set is of the same
  -- type as the first one.
  -- For some instances of "UnorderedList" this may have a more efficient 
  -- implementation.
  --
  intersection' :: c -> c -> c
  intersection' = intersection

  -- | Default method.
  -- Returns a new set from @[]@.
  -- 
  newUnorderedList :: [e] -> c
  newUnorderedList = DS.new

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
  -- For some instances of "UnorderedList" this may have a more efficient 
  -- implementation.
  --
  union' :: c -> c -> c
  union' = union


--------------------------------------------------------------------------------
-- UnorderedList -> Monoid
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (UnorderedList c e) => Semigroup c where
  (<>) = mappend

instance {-# OVERLAPPABLE #-} (UnorderedList c e) => Monoid c where
  mempty  = newUnorderedList []
  mappend = union
