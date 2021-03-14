{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.List where
  
import           Control.Monad (ap, join, liftM2)
import           Data.List as L (maximumBy, sort, sortOn)
import           Data.Maybe (Maybe(..), isJust, maybe)

import           MMZKDS.MDS (MDS(..))


--------------------------------------------------------------------------------
-- List Type Class
--------------------------------------------------------------------------------

-- | 'List' is a type class for immutable sequential (list) data structures, 
-- with methods including random access, addition, deletion and so on.
-- It is based on the Java List Interface.
-- Minimal implementation requires @add@, @clear@, @delete@, @get@, @indicesOf@,
-- @newList@, @set@, @size@ and @subList@.
-- Default methods include @append@, @contains@, @indexOf@, @isNull@, 
-- @lastIndexOf@, @pop@, @popFront@, @push@, @remove@, @sort@, @sortOn@, 
-- @toList@ and @update@.
-- For functional operations, one can either create an 'Monad' instance, or
-- "stream" the list structure with @toList@, apply the functions, then 
-- "collect" it back with "@newList@".
-- For methods that involves indices or elements, if the method changes the size
-- of the list (e.g. @add@ or @pop@), the list is the last argument; if the
-- method does not change the size (e.g. @get@ or @set@), the list is the first
-- argument.
--
class List l e where
  -- | Adds an element into the list structure.
  -- Takes an @Int@ as index, an element and a list, returns a list that inserts
  -- the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  add :: Int -> e -> l e -> l e

  -- | Returns an empty list.
  -- Note that it is not guaranteed that any element is physically removed from
  -- the list structure; the method may simply render all elements inaccessible.
  --
  clear :: l e -> l e

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

  -- | Returns a new list structure with the elements of a 'Foldable' instance,
  -- for example, @[a]@.
  --
  newList :: Foldable f => f e -> l e 

  -- | Takes a list structure, an @Int@ as index and an element, returns a list
  -- that overwrites the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  --
  set :: l e -> Int -> e -> l e

  -- | Returns the size (length) of the list structure.
  --
  size :: l e -> Int

  -- | Returns a sub-list of the list structure from the first argument 
  -- (inclusive) to the second argument (exclusive).
  --
  subList :: Int -> Int -> l e -> l e

  -- | Return the list representation of the list structure.
  toList :: l e -> [e]


  -- | Default method.
  -- Insert an element to the end of the list structure.
  --
  append :: e -> l e -> l e 
  append = flip (join (flip . add . size))

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
  -- Returns @True@ if and only if the list structure is empty.
  --
  isNull :: l e -> Bool
  isNull = (== 0) . size

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

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns a tuple containing the removed element and a list that removes the 
  -- given element at the index.
  -- If the list is empty, returns a typle of @Nothing@ and the original list.
  --
  pop :: l e -> (Maybe e, l e)
  pop = join (delete . (+ (-1)) . size)

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
  push = add 0

  -- | Default method.
  -- Removes the first occurrence of an element from the list structure, and
  -- returns a tuple of that element and the list without that element.
  -- If the element does not appear in the list, returns a tuple of @Nothing@
  -- and the original list.
  --
  remove :: Eq e => e -> l e -> (Maybe e, l e)
  remove e l 
    = maybe (Nothing, l) (flip delete l) (indexOf l e)

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

-- | 'MList' is a type class for mutable sequential data structures based on the
-- @ST@-monad, with 
-- methods including random access, addition, deletion, find index and so on.
-- It is based on the Java List Interface.  
-- Minimal implementation requires @mAdd@, @mClear@, @mDelete@, @mGet@, 
-- @mIndicesOf@, @mSet@, @mSize@, @mSortOn@, @mSubList@, @mToList@ and 
-- @newWList@.
-- Default methods include @mAppend@, @mContains@, @mIndexof@, @mIsNull@, 
-- @mLastIndexOf@, @mPop@, @mPopFront@, @mPush@, @mRemove@, @mSort@ and 
-- @mUpdate@.
-- For methods that involves indices or elements, if the method changes the size
-- of the list (e.g. @mAdd@ or @mPop@), the list is the last argument; if the
-- method does not change the size (e.g. @mGet@ or @mSet@), the list is the 
-- first argument.
--
class (Monad (m s), MDS (l e) s) => MList l e m s where
  -- | Adds an element into the list structure.
  -- Takes an Int as index, an element and a list, modifies the list by
  -- inserting the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  --
  mAdd :: Int -> e -> l e s -> m s ()

  -- | Makes the list empty, i.e. remove all elements.
  -- Note that it is not guaranteed that any element is physically removed from
  -- the list structure; the method may simply render all elements inaccessible.
  --
  mClear :: l e s -> m s ()

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

  -- | Takes a list structure, an @Int@ as index and an element, modifies the list
  -- by overwriting the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  --
  mSet :: l e s -> Int -> e -> m s ()

  -- | Returns the size (length) of the list structure.
  --
  mSize :: l e s -> m s Int

  -- | Sort the list structure by a ordering function.
  --
  mSortOn :: Ord o => (e -> o) -> l e s -> m s ()

  -- | Returns a sub-list of the list structure from the first argument 
  -- (inclusive) to the second argument (exclusive).
  --
  mSubList :: Int -> Int -> l e s -> m s (l e s)

  -- | Returns the default list (@[a]@) representation of the list structure.
  --
  mToList :: l e s -> m s [e]

  -- | Returns a new list structure with the elements of a 'Foldable' instance,
  -- for example, @[a]@.
  --
  newMList :: Foldable f => f e -> m s (l e s)

  -- | Default method.
  -- Insert an element to the end of the list structure.
  --
  mAppend :: e -> l e s -> m s ()
  mAppend = liftM2 (>>=) mSize . flip . flip mAdd

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
  -- Returns @True@ if and only if the list structure is empty.
  --
  mIsNull :: l e s -> m s Bool
  mIsNull = (>>= return . (== 0)) . mSize

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the orignal list is unmodified.
  --
  mPop :: l e s -> m s (Maybe e)
  mPop ml = do
    l <- mSize ml
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
  mPush = mAdd 0

  -- | Default method.
  -- Removes the first occurrence of an element from the list structure, and
  -- returns that element while removing the element.
  -- If the element does not appear in the list, returns @Nothing@.
  --
  mRemove :: Eq e => e -> l e s -> m s (Maybe e)
  mRemove e ml = do
    index <- ml `mIndexOf` e
    maybe (return Nothing) (flip mDelete ml) index

  -- | Default method.
  -- Sort the list structure in the default ordering of its elements.
  --
  {-# INLINE mSort #-}
  mSort :: Ord e => l e s -> m s ()
  mSort = mSortOn id

  -- | Default method.
  -- Takes a list structure, an @Int@ as index, and a function updating an
  -- element, modifies the list by updating the element at the index by the
  -- given function.
  -- Returns an error if the index of out of bound.
  --
  mUpdate :: l e s -> Int -> (e -> e) -> m s ()
  mUpdate ml index f = do
    v <- mGet ml index
    mSet ml index (f v)


instance {-# OVERLAPPABLE #-} (Eq a, List l a) => Eq (l a) where
  l == l' 
    = ls == ls' && and (map (liftM2 (==) (l `get`) (l' `get`)) [0..(ls - 1)])
    where
      ls  = size l
      ls' = size l'
