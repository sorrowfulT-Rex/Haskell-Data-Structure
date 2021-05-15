{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module MMZKDS.Class.MList (MList(..)) where

import           Control.Monad (forM, liftM2)
import           Data.Maybe (fromJust, isJust, listToMaybe)

import           MMZKDS.Class.MDS as MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- MList Type Class
--------------------------------------------------------------------------------

-- | 'MList' is a type class for mutable sequential data structures based on the
-- @ST@-monad, with methods including random access, addition, deletion, finding 
-- index and so on.
-- It is based on the Java List Interface.  
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- The list structure should have consecutive index from 0 to its size - 1.
-- Minimal implementation requires @delete@, @get@, @insert@, @indicesOf@,
-- @set@, @size@, @sortOn@ and @subList@.
-- Default methods include @append@, @contains@, @indexof@, @isNull@, 
-- @lastIndexOf@, @newList@, @pop@, @popFront@, @push@, @remove@, @removeAll@,
-- @removeLast@, @sort@, @toList@, @update@ and @update'@.
-- For methods that involves indices or elements, if the method changes the size
-- of the list (e.g. @add@ or @pop@), the list is the last argument; if the
-- method does not change the size (e.g. @get@ or @set@), the list is the 
-- first argument.
--
class (Monad (m s), MDS l m s, MDSCons [e] l m s) 
  => MList l e m s | l -> e where
  -- | Adds an element into the list structure.
  -- Takes an Int as index, an element and a list, modifies the list by
  -- inserting the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  insert :: Int -> e -> l s -> m s ()

  -- | Removes an element into the list structure.
  -- Takes an @Int@ as index and a list, returns the removed element and deletes
  -- the element from the list.
  -- If the index is out of bound, returns @Nothing@ and the orignal list is 
  -- unmodified.
  --
  delete :: Int -> l s -> m s (Maybe e)

  -- | Returns the element of the list structure at the given index.
  -- Returns an error if the index of out of bound.
  -- It is usally used as an infix operator.
  --
  get :: l s -> Int -> m s e

  -- | Takes a list structure and an element, returns a list containing all 
  -- indices that has the element from least to greatest.
  -- Usually used as an infix function.
  --
  indicesOf :: Eq e => l s -> e -> m s [Int]

  -- | Takes a list structure, an @Int@ as index and an element, modifies the 
  -- list by overwriting the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  --
  set :: l s -> Int -> e -> m s ()

  -- | Sort the list structure by a ordering function.
  --
  sortOn :: Ord o => (e -> o) -> l s -> m s ()

  -- | Returns a sub-list of the list structure from the first argument 
  -- (inclusive) to the second argument (exclusive).
  --
  subList :: Int -> Int -> l s -> m s (l s)

  -- | Default method.
  -- Insert an element to the end of the list structure.
  --
  append :: e -> l s -> m s ()
  append = liftM2 (>>=) MDS.size . flip . flip insert

  -- | Default method.
  -- Takes a list structure and an element, returns @True@ if and only if the
  -- element is in the list.
  --
  contains :: Eq e => l s -> e -> m s Bool
  contains = (fmap isJust .) . indexOf

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- first occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  indexOf :: Eq e => l s -> e -> m s (Maybe Int)
  indexOf = (fmap listToMaybe .) . indicesOf

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- last occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  lastIndexOf :: Eq e => l s -> e -> m s (Maybe Int)
  lastIndexOf = (fmap (listToMaybe . reverse) .) . indicesOf

  -- | Default method.
  -- Returns a new list structure with from @[]@.
  --
  newList :: [e] -> m s (l s)
  newList = MDS.new

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the orignal list is unmodified.
  --
  pop :: l s -> m s (Maybe e)
  pop ml = do
    l <- MDS.size ml
    delete (l - 1) ml

  -- | Default method.
  -- Removes the first element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the orignal list is unmodified.
  --
  popFront :: l s -> m s (Maybe e)
  popFront = delete 0

  -- | Default method.
  -- Insert an element to the front of the list structure.
  --
  push :: e -> l s -> m s ()
  push = insert 0

  -- | Default method.
  -- Removes the first occurrence of an element from the list structure, and
  -- returns that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  remove :: Eq e => e -> l s -> m s (Maybe e)
  remove e ml = do
    index <- ml `indexOf` e
    maybe (return Nothing) (`delete` ml) index

  -- | Default method.
  -- Removes all occurrences of an element from the list structure, and returns 
  -- that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  removeAll :: Eq e => e -> l s -> m s [e]
  removeAll e ml = do
    indices <- ml `indicesOf` e
    forM (zip indices [0..]) $ 
      \(i, offset) -> fromJust <$> delete (i - offset) ml

  -- | Default method.
  -- Removes the last occurrence of an element from the list structure, and
  -- returns that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  removeLast :: Eq e => e -> l s -> m s (Maybe e)
  removeLast e ml = do
    index <- ml `lastIndexOf` e
    maybe (return Nothing) (`delete` ml) index

  -- | Default method.
  -- Sort the list structure in the default ordering of its elements.
  --
  {-# INLINE sort #-}
  sort :: Ord e => l s -> m s ()
  sort = sortOn id

  -- | Returns the default list (@[]@) representation of the list structure.
  --
  toList :: l s -> m s [e]
  toList = MDS.finish

  -- | Default method.
  -- Takes a list structure, an @Int@ as index, and a function updating an
  -- element, modifies the list by updating the element at the index by the
  -- given function.
  -- Returns an error if the index of out of bound.
  --
  update :: l s -> Int -> (e -> e) -> m s ()
  update ml index f = do
    v <- get ml index
    set ml index $ f v

  -- | Default method.
  -- Strict version of @mUpdate@.
  --
  update' :: l s -> Int -> (e -> e) -> m s ()
  update' ml index f = do
    v <- get ml index
    set ml index $! f v
