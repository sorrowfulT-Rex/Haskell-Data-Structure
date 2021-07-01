{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Class.UnorderedList (UnorderedList(..)) where

import           Control.Monad (liftM2)
import           Data.List (foldl')
import           Data.Maybe (maybe)

import           MMZKDS.Class.DS as DS (DS(..), DSCons(..))


--------------------------------------------------------------------------------
-- UnorderedList Type Class
--------------------------------------------------------------------------------

-- | 'UnorderedList' is a type class for immutable list structures where the 
-- elements are not ordered by index, but by its intrinsic order.
-- It is similar to a list that can have duplicates.
-- It is expected that the elements are instances of 'Ord'.  
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @add@, @contains@, @getNthMin@, and @remove@.
-- Default methods include @count@, @difference@, @difference'@, @dropAny@, 
-- @findAny@, @getMax@, @getNthMax@, @getMin@, @intersection@, @intersection'@, 
-- @newUnorderedList@, @toList@, @union@ and @union'@.
--
class (DS l, DSCons [e] l) => UnorderedList l e | l -> e where
  -- | Adds an element into the list.
  add :: l -> e -> l

  -- | Tests if the element is in the list.
  -- Returns the range of position of this element in the list. If it does not
  -- exist, returns Nothing.
  --
  contains :: l -> e -> Maybe (Int, Int)

  -- | Gets the n-th minimum element in the list.
  --w
  getNthMin :: l -> Int -> Maybe e

  -- | Removes the given element from the list.
  -- Returns a tuple consisting of the removed element (or Nothing, if the
  -- element is not in the list), and the list without the element.
  --
  remove :: l -> e -> (Maybe e, l)

  -- | Default method.
  -- Counts the number of occurrences of an element in the list.
  --
  count :: l -> e -> Int
  count l e 
    = maybe 0 (\(x, y) -> y - x + 1) $ contains l e

  -- | Default method.
  -- Computes the difference of two sets.
  --
  difference :: forall l1. DSCons [e] l1 => l -> l1 -> l
  difference = (. (DS.finish :: l1 -> [e])) . foldl' ((snd .) . remove)

  -- | Default method.
  -- Computes the difference of two sets, where the second list is of the same
  -- type as the first one.
  -- For some instances of "UnorderedList" this may have a more efficient 
  -- implementation.
  --
  difference' ::  l -> l -> l
  difference' = difference

  -- | Default method.
  -- Removes an element from the list.
  -- Returns a tuple consisting of the removed element (or Nothing, if the
  -- element is not in the list), and the list without the element.
  -- Does not guarantee which element is removed.
  -- 
  dropAny :: l -> (Maybe e, l)
  dropAny s 
    = maybe (Nothing, s) (remove s) (findAny s)

  -- | Default method.
  -- Returns an element from the list if it is non-empty.
  -- Does not guarantee which element is returned.
  -- 
  findAny :: l -> Maybe e
  findAny = flip getNthMin 1

  -- | Gets the maximum element in the list.
  --
  getMax :: l -> Maybe e
  getMax = flip getNthMax 1

  -- | Gets the nth maximum element in the list.
  --
  getNthMax :: l -> Int -> Maybe e
  getNthMax l i = getNthMin l (size l - i + 1)

  -- | Gets the minimum element in the list.
  --
  getMin :: l -> Maybe e
  getMin = flip getNthMin 1

  -- | Default method.
  -- Computes the intersection of two sets.
  --
  intersection :: forall l1. DSCons [e] l1 => l -> l1 -> l
  intersection = liftM2 (.) difference difference

  -- | Default method.
  -- Computes the intersection of two sets, where the second list is of the same
  -- type as the first one.
  -- For some instances of "UnorderedList" this may have a more efficient 
  -- implementation.
  --
  intersection' :: l -> l -> l
  intersection' = intersection

  -- | Default method.
  -- Returns a new list from @[]@.
  -- 
  newUnorderedList :: [e] -> l
  newUnorderedList = DS.new

  -- | Default method.
  -- Return the list representation of the list.
  -- 
  toList :: l -> [e]
  toList = DS.finish

  -- | Default method.
  -- Computes the union of two sets.
  --
  union :: forall l1. DSCons [e] l1 => l -> l1 -> l
  union = (. (DS.finish :: l1 -> [e])) . foldl' add

  -- | Default method.
  -- Computes the union of two sets, where the second list is of the same
  -- type as the first one.
  -- For some instances of "UnorderedList" this may have a more efficient 
  -- implementation.
  --
  union' :: l -> l -> l
  union' = union


--------------------------------------------------------------------------------
-- UnorderedList -> Monoid
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (UnorderedList l e) => Semigroup l where
  (<>) = mappend

instance {-# OVERLAPPABLE #-} (UnorderedList l e) => Monoid l where
  mempty  = newUnorderedList []
  mappend = union
