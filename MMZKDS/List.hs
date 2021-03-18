{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.List where

import           Control.Monad (ap, forM, join, liftM2, (<=<))
import           Data.List as L (foldl', maximumBy, sort, sortOn)
import           Data.Maybe (Maybe(..), fromJust, isJust, maybe)

import           MMZKDS.DS as DS (DS(..), DSCons(..))
import           MMZKDS.MDS as MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- List Type Class
--------------------------------------------------------------------------------

-- | 'List' is a type class for immutable sequential (list) data structures, 
-- with methods including random access, addition, deletion and so on.
-- It is based on the Java List Interface.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- The list structure should have consecutive index from 0 to its size - 1.
-- Minimal implementation requires  @delete@, @get@, @indicesOf@, @insert@,
-- @set@, and @subList@.
-- Default methods include @append@, @contains@, @indexOf@, @isNull@, 
-- @lastIndexOf@, @newList@, @pop@, @popFront@, @push@, @remove@, @removeAll@,
-- @removeLast@, @sort@, @sortOn@, @toList@, @update@ and @update'@.
-- For functional operations, one can either create an 'Monad' instance, or
-- "stream" the list structure with @toList@, apply the functions, then 
-- "collect" it back with "@newList@".
-- For methods that involves indices or elements, if the method changes the size
-- of the list (e.g. @add@ or @pop@), the list is the last argument; if the
-- method does not change the size (e.g. @get@ or @set@), the list is the first
-- argument.
--
class (DS (l e), DSCons [e] (l e)) => List l e where
  -- | Adds an element into the list structure.
  -- Takes an @Int@ as index, an element and a list, returns a list that inserts
  -- the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  insert :: Int -> e -> l e -> l e

  -- | Removes an element from the list structure.
  -- Takes an @Int@ as index and a list, returns a tuple containing the removed 
  -- element and a list that removes the given element at the index.
  -- If the index is out of bound, returns a typle of @Nothing@ and the original
  -- list.
  --
  delete :: Int -> l e -> (Maybe e, l e)

  -- | Returns the element of the list structure at the given index.
  -- Returns an error if the index of out of bound.
  -- It is usally used as an infix operator.
  --
  get :: l e -> Int -> e

  -- | Takes a list structure and an element, returns a list containing all 
  -- indices that has the element from least to greatest.
  -- Usually used as an infix function.
  --
  indicesOf :: Eq e => l e -> e -> [Int]

  -- | Takes a list structure, an @Int@ as index and an element, returns a list
  -- that overwrites the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  --
  set :: l e -> Int -> e -> l e

  -- | Returns a sub-list of the list structure from the first argument 
  -- (inclusive) to the second argument (exclusive).
  --
  subList :: Int -> Int -> l e -> l e

  -- | Default method.
  -- Insert an element to the end of the list structure.
  --
  append :: e -> l e -> l e
  append = flip (join (flip . insert . DS.size))

  -- | Default method.
  -- Takes a list structure and an element, returns @True@ if and only if the
  -- element is in the list.
  --
  contains :: Eq e => l e -> e -> Bool
  contains = (isJust .) . indexOf

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- first occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  indexOf :: Eq e => l e -> e -> Maybe Int
  indexOf l e
    | notFound  = Nothing
    | otherwise = Just $ head indices
    where
      notFound = null indices
      indices  = indicesOf l e

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- last occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  lastIndexOf :: Eq e => l e -> e -> Maybe Int
  lastIndexOf l e
    | notFound  = Nothing
    | otherwise = Just $ last indices
    where
      notFound = null indices
      indices  = indicesOf l e

  -- | Default Method
  -- Returns a new list structure with from @[]@.
  --
  newList :: [e] -> l e
  newList = DS.new

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns a tuple containing the removed element and a list that removes the 
  -- given element at the index.
  -- If the list is empty, returns a typle of @Nothing@ and the original list.
  --
  pop :: l e -> (Maybe e, l e)
  pop = join (delete . (+ (-1)) . DS.size)

  -- | Default method.
  -- Removes the fisrt element from the list structure.
  -- Returns a tuple containing the removed element and a list that removes the 
  -- given element at the index.
  -- If the list is empty, returns a typle of @Nothing@ and the original list.
  --
  popFront :: l e -> (Maybe e, l e)
  popFront = delete 0

  -- | Default method.
  -- Insert an element to the front of the list structure.
  --
  push :: e -> l e -> l e
  push = insert 0

  -- | Default method.
  -- Return the list representation of the list structure.
  toList :: l e -> [e]
  toList = DS.finish

  -- | Default method.
  -- Removes the first occurrence of an element from the list structure, and
  -- returns a tuple of that element and the list without that element.
  -- If the element does not appear in the list, returns a tuple of @Nothing@
  -- and the original list.
  --
  remove :: Eq e => e -> l e -> (Maybe e, l e)
  remove e l
    = maybe (Nothing, l) (`delete` l) (indexOf l e)

  -- | Default method.
  -- Removes the all occurrences of an element from the list structure, and
  -- returns a tuple of that element and the list without that element.
  -- If the element does not appear in the list, returns a tuple of @Nothing@
  -- and the original list.
  --
  removeAll :: Eq e => e -> l e -> ([e], l e)
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
  removeLast :: Eq e => e -> l e -> (Maybe e, l e)
  removeLast e l
    = maybe (Nothing, l) (`delete` l) (lastIndexOf l e)

  -- | Default method.
  -- Sort the list structure in the default ordering of its elements.
  --
  sort :: Ord e => l e -> l e
  sort = newList . L.sort . toList

  -- | Default method.
  -- Sort the list structure by a ordering function.
  --
  sortOn :: Ord o => (e -> o) -> l e -> l e
  sortOn = (. toList) . (newList .) . L.sortOn

  -- | Default method.
  -- Takes a list structure, an Int as index, and a function updating an
  -- element, returns a list that updates the element at the index by the given
  -- function.
  -- Returns an error if the index of out of bound.
  --
  update :: l e -> Int -> (e -> e) -> l e
  update = ap (ap . ((.) .) . set) ((flip id .) . get)

  -- | Default method.
  -- Strict version of @update@.
  --
  update' :: l e -> Int -> (e -> e) -> l e
  update' = ap (ap . (((.) . ($!)) .) . set) ((flip id .) . get)

-- | 'MList' is a type class for mutable sequential data structures based on the
-- @ST@-monad, with 
-- methods including random access, addition, deletion, find index and so on.
-- It is based on the Java List Interface.  
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- The list structure should have consecutive index from 0 to its size - 1.
-- Minimal implementation requires @mDelete@, @mGet@, @mInsert@, @mIndicesOf@,
-- @mSet@, @mSize@, @mSortOn@ and @mSubList@.
-- Default methods include @mAppend@, @mContains@, @mIndexof@, @mIsNull@, 
-- @mLastIndexOf@, @mNewList@, @mPop@, @mPopFront@, @mPush@, @mRemove@,
-- @mRemoveAll@, @mRemoveLast@, @mSort@, @mToList@, @mUpdate@ and @mUpdate'@.
-- For methods that involves indices or elements, if the method changes the size
-- of the list (e.g. @mAdd@ or @mPop@), the list is the last argument; if the
-- method does not change the size (e.g. @mGet@ or @mSet@), the list is the 
-- first argument.
--
class (Monad (m s), MDS (l e) m s, MDSCons [e] (l e) m s) => MList l e m s where
  -- | Adds an element into the list structure.
  -- Takes an Int as index, an element and a list, modifies the list by
  -- inserting the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  mInsert :: Int -> e -> l e s -> m s ()

  -- | Removes an element into the list structure.
  -- Takes an @Int@ as index and a list, returns the removed element and deletes
  -- the element from the list.
  -- If the index is out of bound, returns @Nothing@ and the orignal list is 
  -- unmodified.
  --
  mDelete :: Int -> l e s -> m s (Maybe e)

  -- | Returns the element of the list structure at the given index.
  -- Returns an error if the index of out of bound.
  -- It is usally used as an infix operator.
  --
  mGet :: l e s -> Int -> m s e

  -- | Takes a list structure and an element, returns a list containing all 
  -- indices that has the element from least to greatest.
  -- Usually used as an infix function.
  --
  mIndicesOf :: Eq e => l e s -> e -> m s [Int]

  -- | Takes a list structure, an @Int@ as index and an element, modifies the 
  -- list by overwriting the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  --
  mSet :: l e s -> Int -> e -> m s ()

  -- | Sort the list structure by a ordering function.
  --
  mSortOn :: Ord o => (e -> o) -> l e s -> m s ()

  -- | Returns a sub-list of the list structure from the first argument 
  -- (inclusive) to the second argument (exclusive).
  --
  mSubList :: Int -> Int -> l e s -> m s (l e s)

  -- | Default method.
  -- Insert an element to the end of the list structure.
  --
  mAppend :: e -> l e s -> m s ()
  mAppend = liftM2 (>>=) MDS.size . flip . flip mInsert

  -- | Default method.
  -- Takes a list structure and an element, returns @True@ if and only if the
  -- element is in the list.
  --
  mContains :: Eq e => l e s -> e -> m s Bool
  mContains = (fmap isJust .) . mIndexOf

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- first occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  mIndexOf :: Eq e => l e s -> e -> m s (Maybe Int)
  mIndexOf ml e = do
    indices <- mIndicesOf ml e
    return $ if null indices
      then Nothing
      else Just $ head indices

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- last occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  --
  mLastIndexOf :: Eq e => l e s -> e -> m s (Maybe Int)
  mLastIndexOf ml e = do
    indices <- mIndicesOf ml e
    return $ if null indices
      then Nothing
      else Just $ last indices

  -- | Default method.
  -- Returns a new list structure with from @[]@.
  --
  mNewList :: [e] -> m s (l e s)
  mNewList = MDS.new

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the orignal list is unmodified.
  --
  mPop :: l e s -> m s (Maybe e)
  mPop ml = do
    l <- MDS.size ml
    mDelete (l - 1) ml

  -- | Default method.
  -- Removes the first element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the orignal list is unmodified.
  --
  mPopFront :: l e s -> m s (Maybe e)
  mPopFront = mDelete 0

  -- | Default method.
  -- Insert an element to the front of the list structure.
  --
  mPush :: e -> l e s -> m s ()
  mPush = mInsert 0

  -- | Default method.
  -- Removes the first occurrence of an element from the list structure, and
  -- returns that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  mRemove :: Eq e => e -> l e s -> m s (Maybe e)
  mRemove e ml = do
    index <- ml `mIndexOf` e
    maybe (return Nothing) (`mDelete` ml) index

  -- | Default method.
  -- Removes all occurrences of an element from the list structure, and returns 
  -- that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  mRemoveAll :: Eq e => e -> l e s -> m s [e]
  mRemoveAll e ml = do
    indices <- ml `mIndicesOf` e
    forM (zip indices [0..]) $ 
      \(i, offset) -> fromJust <$> mDelete (i - offset) ml

  -- | Default method.
  -- Removes the last occurrence of an element from the list structure, and
  -- returns that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  mRemoveLast :: Eq e => e -> l e s -> m s (Maybe e)
  mRemoveLast e ml = do
    index <- ml `mLastIndexOf` e
    maybe (return Nothing) (`mDelete` ml) index

  -- | Default method.
  -- Sort the list structure in the default ordering of its elements.
  --
  {-# INLINE mSort #-}
  mSort :: Ord e => l e s -> m s ()
  mSort = mSortOn id

  -- | Returns the default list (@[]@) representation of the list structure.
  --
  mToList :: l e s -> m s [e]
  mToList = MDS.finish

  -- | Default method.
  -- Takes a list structure, an @Int@ as index, and a function updating an
  -- element, modifies the list by updating the element at the index by the
  -- given function.
  -- Returns an error if the index of out of bound.
  --
  mUpdate :: l e s -> Int -> (e -> e) -> m s ()
  mUpdate ml index f = do
    v <- mGet ml index
    mSet ml index $ f v

  -- | Default method.
  -- Strict version of @mUpdate@.
  --
  mUpdate' :: l e s -> Int -> (e -> e) -> m s ()
  mUpdate' ml index f = do
    v <- mGet ml index
    mSet ml index $! f v


--------------------------------------------------------------------------------
-- List -> Eq
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (Eq a, List l a) => Eq (l a) where
  l == l'
    = ls == ls' && all (liftM2 (==) (l `get`) (l' `get`)) [0..(ls - 1)]
    where
      ls  = DS.size l
      ls' = DS.size l'
