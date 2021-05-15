{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Class.List (List(..)) where

import           Control.Monad (ap, join, liftM2)
import           Data.List as L (foldl', sort, sortOn)
import           Data.Maybe (isJust, listToMaybe)

import           MMZKDS.Class.DS as DS (DS(..), DSCons(..))


--------------------------------------------------------------------------------
-- List Type Class
--------------------------------------------------------------------------------

-- | 'List' is a type class for immutable sequential (list) data structures, 
-- with methods including random access, addition, deletion, finding index and 
-- so on.
-- It is based on the Java List Interface.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- The list structure should have consecutive index from 0 to its size - 1.
-- Minimal implementation requires @delete@, @get@, @indicesOf@, @insert@,
-- @set@, and @subList@.
-- Default methods include @append@, @contains@, @indexOf@, @isNull@, 
-- @lastIndexOf@, @newList@, @pop@, @popFront@, @push@, @remove@, @removeAll@,
-- @removeLast@, @sort@, @sortOn@, @toList@, @update@ and @update'@.
-- For functional operations, one can either create a 'Monad' instance, or
-- "stream" the list structure with @toList@, apply the functions, then 
-- "collect" it back with "@newList@".
-- For methods that involves indices or elements, if the method changes the size
-- of the list (e.g. @add@ or @pop@), the list is the last argument; if the
-- method does not change the size (e.g. @get@ or @set@), the list is the first
-- argument.
--
class (DS l, DSCons [e] l) => List l e | l -> e where
  -- | Adds an element into the list structure.
  -- Takes an @Int@ as index, an element and a list, returns a list that inserts
  -- the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  insert :: Int -> e -> l -> l

  -- | Removes an element from the list structure.
  -- Takes an @Int@ as index and a list, returns a tuple containing the removed 
  -- element and a list that removes the given element at the index.
  -- If the index is out of bound, returns a typle of @Nothing@ and the original
  -- list.
  --
  delete :: Int -> l -> (Maybe e, l)

  -- | Returns the element of the list structure at the given index.
  -- Returns an error if the index of out of bound.
  -- It is usally used as an infix operator.
  --
  get :: l -> Int -> e

  -- | Takes a list structure and an element, returns a list containing all 
  -- indices that has the element from least to greatest.
  -- Usually used as an infix function.
  --
  indicesOf :: Eq e => l -> e -> [Int]

  -- | Takes a list structure, an @Int@ as index and an element, returns a list
  -- that overwrites the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  --
  set :: l -> Int -> e -> l

  -- | Returns a sub-list of the list structure from the first argument 
  -- (inclusive) to the second argument (exclusive).
  --
  subList :: Int -> Int -> l -> l

  -- | Default method.
  -- Insert an element to the end of the list structure.
  --
  append :: e -> l -> l
  append = flip (join (flip . insert . DS.size))

  -- | Default method.
  -- Takes a list structure and an element, returns @True@ if and only if the
  -- element is in the list.
  --
  contains :: Eq e => l -> e -> Bool
  contains = (isJust .) . indexOf

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- first occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  indexOf :: Eq e => l -> e -> Maybe Int
  indexOf = (listToMaybe .) . indicesOf

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- last occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  lastIndexOf :: Eq e => l -> e -> Maybe Int
  lastIndexOf = ((listToMaybe . reverse) .) . indicesOf

  -- | Default Method
  -- Returns a new list structure with from @[]@.
  --
  newList :: [e] -> l
  newList = DS.new

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns a tuple containing the removed element and a list that removes the 
  -- given element at the index.
  -- If the list is empty, returns a typle of @Nothing@ and the original list.
  --
  pop :: l -> (Maybe e, l)
  pop = join (delete . (+ (-1)) . DS.size)

  -- | Default method.
  -- Removes the fisrt element from the list structure.
  -- Returns a tuple containing the removed element and a list that removes the 
  -- given element at the index.
  -- If the list is empty, returns a typle of @Nothing@ and the original list.
  --
  popFront :: l -> (Maybe e, l)
  popFront = delete 0

  -- | Default method.
  -- Insert an element to the front of the list structure.
  --
  push :: e -> l -> l
  push = insert 0

  -- | Default method.
  -- Return the list representation of the list structure.
  toList :: l -> [e]
  toList = DS.finish

  -- | Default method.
  -- Removes the first occurrence of an element from the list structure, and
  -- returns a tuple of that element and the list without that element.
  -- If the element does not appear in the list, returns a tuple of @Nothing@
  -- and the original list.
  --
  remove :: Eq e => e -> l -> (Maybe e, l)
  remove e l
    = maybe (Nothing, l) (`delete` l) (indexOf l e)

  -- | Default method.
  -- Removes the all occurrences of an element from the list structure, and
  -- returns a tuple of that element and the list without that element.
  -- If the element does not appear in the list, returns a tuple of @Nothing@
  -- and the original list.
  --
  removeAll :: Eq e => e -> l -> ([e], l)
  removeAll e l
    = let (a, b, _) = foldl' worker ([], l, 0) indices in (a, b)
    where
      indices = l `indicesOf` e
      worker (es, l, offset) i
        = let (Just e', l') = delete (i - offset) l in (e' : es, l', offset + 1)

  -- | Default method.
  -- Removes the last occurrence of an element from the list structure, and
  -- returns a tuple of that element and the list without that element.
  -- If the element does not appear in the list, returns a tuple of @Nothing@
  -- and the original list.
  --
  removeLast :: Eq e => e -> l -> (Maybe e, l)
  removeLast e l
    = maybe (Nothing, l) (`delete` l) (lastIndexOf l e)

  -- | Default method.
  -- Sort the list structure in the default ordering of its elements.
  --
  sort :: Ord e => l -> l
  sort = newList . L.sort . toList

  -- | Default method.
  -- Sort the list structure by a ordering function.
  --
  sortOn :: Ord o => (e -> o) -> l -> l
  sortOn = (. toList) . (newList .) . L.sortOn

  -- | Default method.
  -- Takes a list structure, an Int as index, and a function updating an
  -- element, returns a list that updates the element at the index by the given
  -- function.
  -- Returns an error if the index of out of bound.
  --
  update :: l -> Int -> (e -> e) -> l
  update = ap (ap . ((.) .) . set) ((flip id .) . get)

  -- | Default method.
  -- Strict version of @update@.
  --
  update' :: l -> Int -> (e -> e) -> l
  update' = ap (ap . (((.) . ($!)) .) . set) ((flip id .) . get)


--------------------------------------------------------------------------------
-- List -> Eq
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (Eq a, List (l a) a) => Eq (l a) where
  l == l'
    = ls == ls' && all (liftM2 (==) (l `get`) (l' `get`)) [0..(ls - 1)]
    where
      ls  = DS.size l
      ls' = DS.size l'
