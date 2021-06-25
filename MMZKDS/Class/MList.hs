{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.Class.MList (MList(..)) where

import           Control.Monad (forM, forM_, liftM2)
import           Data.Maybe (fromJust, isJust, listToMaybe)
import           Prelude hiding (concat)

import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- MList Type Class
--------------------------------------------------------------------------------

-- | 'MList' is a type class for mutable sequential data structures based on
-- the @ST@-monad, with methods including random access, addition, deletion,  
-- finding index and so on.
-- It is based on the Java List Interface.  
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- The list structure should have consecutive index from 0 to its size - 1.
-- Minimal implementation requires @delete@, @get@, @insert@, @indicesOf@,
-- @set@, @size@, @sortOn@ and @subList@.
-- Default methods include @append@, @concat@, @concat'@, @contains@,
-- @deleteRange@, @indexof@, @insertAll@, @insertAll'@, @lastIndexOf@,
-- @newList@, @pop@, @popFront@, @push@, @remove@, @removeAll@, @removeLast@,
-- @sort@, @toList@, @update@, @update'@ and @(!)@.
-- 
class (Monad (m s), MDS l m s, MDSCons [e] l m s) 
  => MList l e m s | l -> e where
  -- | Adds an element into the list structure.
  -- Takes an Int as index, an element and a list, modifies the list by
  -- inserting the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  insert :: l s -> Int -> e -> m s ()

  -- | Removes an element into the list structure.
  -- Takes an @Int@ as index and a list, returns the removed element and
  -- deletes the element from the list.
  -- If the index is out of bound, returns @Nothing@ and the orignal list is 
  -- unmodified.
  --
  delete :: l s -> Int -> m s (Maybe e)

  -- | Returns the element of the list structure at the given index.
  -- Returns an error if the index of out of bound.
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
  -- Note that in this method the sort function is the first argument.
  --
  sortOn :: Ord o => (e -> o) -> l s -> m s ()

  -- | Returns a sub-list of the list structure from the first argument 
  -- (inclusive) to the second argument (exclusive).
  --
  subList :: l s -> Int -> Int -> m s (l s)

  -- | Default method.
  -- Insert an element to the end of the list structure.
  --
  append :: l s -> e -> m s ()
  append ml e = size ml >>= flip (insert ml) e

  -- | Default method.
  -- Concatenate two lists, modifying the same one.
  -- 
  concat :: forall l1. (MDS l1 m s, MDSCons [e] l1 m s) 
         => l s 
         -> l1 s 
         -> m s ()
  concat ml es = size ml >>= flip (insertAll ml) es

  -- | Default method.
  -- Concatenate two lists with the same type.
  -- May have more efficient implementation than @concat@, which allows the
  -- second data structure to be arbitrary.
  -- 
  concat' :: l s -> l s -> m s ()
  concat' = concat

  -- | Default method.
  -- Takes a list structure and an element, returns @True@ if and only if the
  -- element is in the list.
  --
  contains :: Eq e => l s -> e -> m s Bool
  contains = (fmap isJust .) . indexOf

  -- | Default method.
  -- Delete all elements between the first argument (inclusive and the second
  -- argument (exclusive), returning the deleted elements.
  -- This is like the opposite of @subList@, but operates on (modifies) the
  -- original list.
  -- 
  deleteRange :: l s -> Int -> Int -> m s [e]
  deleteRange ml inf sup = do
    let inf' = max 0 inf
    len <- size ml
    forM [inf'..(min len sup - 1)] $ const $ fromJust <$> delete ml inf'

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- first occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  indexOf :: Eq e => l s -> e -> m s (Maybe Int)
  indexOf = (fmap listToMaybe .) . indicesOf

  -- | Default method.
  -- Adds all elements in a list to the structure before the given index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  insertAll :: forall l1. (MDS l1 m s, MDSCons [e] l1 m s) 
            => l s 
            -> Int 
            -> l1 s 
            -> m s ()
  insertAll ml index es = do
    xs <- (finish :: l1 s -> m s [e]) es
    forM_ (zip xs [0..]) $ \(e, offset) -> insert ml (index + offset) e

  -- | Default method.
  -- Same as @insertAll@, but specifies the list of new elements to be the same
  -- type of the original list. May have a more efficient implementation.
  -- 
  insertAll' :: l s -> Int -> l s -> m s ()
  insertAll' = insertAll

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
  newList = new

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the original list is 
  -- unmodified.
  --
  pop :: l s -> m s (Maybe e)
  pop ml = do
    l <- size ml
    delete ml (l - 1)

  -- | Default method.
  -- Removes the first element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the original list is 
  -- unmodified.
  --
  popFront :: l s -> m s (Maybe e)
  popFront = flip delete 0

  -- | Default method.
  -- Insert an element to the front of the list structure.
  --
  push :: l s -> e -> m s ()
  push = flip insert 0

  -- | Default method.
  -- Removes the first occurrence of an element from the list structure, and
  -- returns that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  remove :: Eq e => l s -> e -> m s (Maybe e)
  remove ml e = do
    index <- ml `indexOf` e
    maybe (return Nothing) (ml `delete`) index

  -- | Default method.
  -- Removes all occurrences of an element from the list structure, and returns 
  -- that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  removeAll :: Eq e => l s -> e -> m s [e]
  removeAll ml e = do
    indices <- ml `indicesOf` e
    forM (zip indices [0..]) $ 
      \(i, offset) -> fromJust <$> delete ml (i - offset)

  -- | Default method.
  -- Removes the last occurrence of an element from the list structure, and
  -- returns that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  removeLast :: Eq e => l s -> e -> m s (Maybe e)
  removeLast ml e = do
    index <- ml `lastIndexOf` e
    maybe (return Nothing) (ml `delete`) index

  -- | Default method.
  -- Sort the list structure in the default ordering of its elements.
  --
  {-# INLINE sort #-}
  sort :: Ord e => l s -> m s ()
  sort = sortOn id

  -- | Returns the default list (@[]@) representation of the list structure.
  --
  toList :: l s -> m s [e]
  toList = finish

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

  -- | Similar to @get@, but returns an error on out-of-bound error.
  -- It is usally used as an infix operator.
  --
  infix 4 !
  (!) :: l s -> Int -> m s e
  (!) = get
