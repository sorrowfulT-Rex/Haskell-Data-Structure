{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module List where
import           Control.Monad (ap, join, liftM2)
import           Control.Monad.ST.Lazy (ST(..), runST, lazyToStrictST)
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Data.Bits (shiftL)
import           Data.Foldable (toList)
import           Data.Maybe (Maybe(..), isJust)
import           System.IO.Unsafe (unsafePerformIO)


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Utility Function. 
-- Returns an array out of bound error.
outOfBoundError :: Int -> a
outOfBoundError i
  = error $ "Index " ++ show i ++ " is out of bound!"

-- | Utility Function. 
-- Takes the needed length of an array and returns a larger
-- number as the physical length, so that some extra space is provided.
initialSize :: Int -> Int
initialSize = expandedSize . shiftL 1 . ceiling . logBase 2 . fromIntegral

-- | Utility Function. 
-- Takes the current length of an array and returns a larger length.
expandedSize :: Int -> Int
expandedSize = (1 +) . (`div` 2) . (3 *)


--------------------------------------------------------------------------------
-- List Interface
--------------------------------------------------------------------------------

-- | 'List' is a type class for immutable sequential data structures, with 
-- methods including random access, addition, deletion and so on.
-- It is based on the Java List Interface.  
-- Minimal implementation requires @add@, @clear@, @get@, @newList@, @remove@,
-- @set@ and @size@.
-- Default methods include @append@, @isNull@, @pop@, @popFront@, @push@ and
-- @update@.
-- Although it is not part of the class, it is expected for the instance of 
-- 'List' to implement @toList@ in 'Foldable'.
class List l where
  -- | Adds an element into the list structure.
  -- Takes an Int as index, an element and a list, returns a list that inserts
  -- the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  add :: Int -> e -> l e -> l e

  -- | Returns an empty list.
  -- Note that it is not guaranteed that any element is physically removed from
  -- the list structure; the method may simply render all elements inaccessible.
  clear :: l e -> l e

  -- | Returns the element of the list structure at the given index.
  -- Returns an error if the index of out of bound.
  -- It is usally used as an infix operator.
  get :: l e -> Int -> e

  -- | Returns a new list structure with the elements of a 'Foldable' instance,
  -- for example, @[a]@.
  newList :: Foldable f => f e -> l e 

  -- | Removes an element from the list structure.
  -- Takes an Int as index and a list, returns a tuple containing the removed 
  -- element and a list that removes the given element at the index.
  -- If the index is out of bound, returns a typle of @Nothing@ and the original
  -- list.
  remove :: Int -> l e -> (Maybe e, l e)

  -- | Takes a list structure, an Int as index and an element, returns a list
  -- that overwrites the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  set :: l e -> Int -> e -> l e

  -- | Returns the size (length) of the list structure.
  size :: l e -> Int

  -- | Default method.
  -- Insert an element to the end of the list structure.
  append :: e -> l e -> l e 
  append = flip (join (flip . add . size))

  -- | Default method.
  -- Returns @True@ if and only if the list structure is empty.
  isNull :: l e -> Bool
  isNull = (== 0) . size

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns a tuple containing the removed element and a list that removes the 
  -- given element at the index.
  -- If the list is empty, returns a typle of @Nothing@ and the original list.
  pop :: l e -> (Maybe e, l e)
  pop = join (remove . (+ (-1)) . size)

  -- | Default method.
  -- Removes the fisrt element from the list structure.
  -- Returns a tuple containing the removed element and a list that removes the 
  -- given element at the index.
  -- If the list is empty, returns a typle of @Nothing@ and the original list.
  popFront :: l e -> (Maybe e, l e)
  popFront = remove 0

  -- | Default method.
  -- Insert an element to the front of the list structure.
  push :: e -> l e -> l e
  push = add 0

  -- | Default method.
  -- Takes a list structure, an Int as index, and a function updating an
  -- element, returns a list that updates the element at the index by the given
  -- function.
  -- Returns an error if the index of out of bound.
  update :: l e -> Int -> (e -> e) -> l e
  update = ap (ap . ((.) .) . set) ((flip id .) . get)

-- | 'MList' is a type class for mutable sequential data structures, with 
-- methods including random access, addition, deletion, find index and so on.
-- It is based on the Java List Interface.  
-- Minimal implementation requires @mAdd@, @mClear@, @mGet@, @mRemove@, @mSet@, 
-- @mSize@, @mToList@ and @newWList@
-- Default methods include @mAppend@, @mIsNull@, @mPop@, @mPopFront@, @mPush@
--  and @mUpdate@.
class MList l where
  -- | Adds an element into the list structure.
  -- Takes an Int as index, an element and a list, modifies the list by
  -- inserting the given element before the index.
  -- If the index is either larger than the length of the list or less than 0,
  -- the function returns an error.
  mAdd :: Int -> e -> l e s -> ST s ()

  -- | Makes the list empty, i.e. remove all elements.
  -- Note that it is not guaranteed that any element is physically removed from
  -- the list structure; the method may simply render all elements inaccessible.
  mClear :: l e s -> ST s ()

  -- | Returns the element of the list structure at the given index.
  -- Returns an error if the index of out of bound.
  -- It is usally used as an infix operator.
  mGet :: l e s -> Int -> ST s e

  -- | Removes an element into the list structure.
  -- Takes an Int as index and a list, returns the removed element and deletes
  -- the element from the list.
  -- If the index is out of bound, returns @Nothing@ and the orignal list is 
  -- unmodified.
  mRemove :: Int -> l e s -> ST s (Maybe e)

  -- | Takes a list structure, an Int as index and an element, modifies the list
  -- by overwriting the element at the index by the given element.
  -- If the index is out of bound, the function returns an error.
  mSet :: l e s -> Int -> e -> ST s ()

  -- | Returns the size (length) of the list structure.
  mSize :: l e s -> ST s Int

  -- | Returns the default list (@[a]@) representation of the list structure.
  mToList :: l e s -> ST s [e]
  newMList :: Foldable f => f e -> ST s (l e s)

  -- | Default method.
  -- Insert an element to the end of the list structure.
  mAppend :: e -> l e s -> ST s ()
  mAppend = liftM2 (>>=) mSize . flip . flip mAdd

  -- | Default method.
  -- Returns @True@ if and only if the list structure is empty.
  mIsNull :: l e s -> ST s Bool
  mIsNull = (>>= return . (== 0)) . mSize

  -- | Default method.
  -- Removes the last element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the orignal list is unmodified.
  mPop :: l e s -> ST s (Maybe e)
  mPop mal = do
    l <- mSize mal
    mRemove (l - 1) mal

  -- | Default method.
  -- Removes the first element from the list structure.
  -- Returns the removed element and deletes the element from the list.
  -- If the list is empty, returns @Nothing@ and the orignal list is unmodified.
  mPopFront :: l e s -> ST s (Maybe e)
  mPopFront = mRemove 0

  -- | Default method.
  -- Insert an element to the front of the list structure.
  mPush :: e -> l e s -> ST s ()
  mPush = mAdd 0

  -- | Default method.
  -- Takes a list structure, an Int as index, and a function updating an
  -- element, modifies the list by updating the element at the index by the
  -- given function.
  -- Returns an error if the index of out of bound.
  mUpdate :: l e s -> Int -> (e -> e) -> ST s ()
  mUpdate mal index f = do
    v <- mGet mal index
    mSet mal index (f v)


--------------------------------------------------------------------------------
-- List With Eq
--------------------------------------------------------------------------------

-- | 'ListEq' is a type class providing operations that only make sense to 
-- immutable list structures containing instances of 'Eq', such as finding index
--  and checking existence of a given element in the structure.
-- Minimal implementation requires @indicesOf@.
-- Default methods include @indexOf@ and @contains@.
class (List l, Eq e) => ListEq l e where
  -- | Takes a list structure and an element, returns a list containing all 
  -- indices that has the element.
  -- Usually used as an infix function.
  indicesOf :: l e -> e -> [Int]

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- first occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  indexOf :: l e -> e -> Maybe Int
  indexOf l e
    | notFound  = Nothing
    | otherwise = Just $ head indices
    where
      notFound = null indices
      indices  = indicesOf l e

  -- | Default method.
  -- Takes a list structure and an element, returns @True@ if and only if the
  -- element is in the list.
  contains :: l e -> e -> Bool
  contains = (isJust .) . indexOf

-- | 'ListEq' is a type class providing operations that only make sense to 
-- mmutable list structures containing instances of 'Eq', such as finding index
--  and checking existence of a given element in the structure.
-- Minimal implementation requires @mIndicesOf@.
-- Default methods include @mIndexOf@ and @mContains@.
class (MList l, Eq e) => MListEq l e s where
  -- | Takes a list structure and an element, returns a list containing all 
  -- indices that has the element.
  -- Usually used as an infix function.
  mIndicesOf :: l e s -> e -> ST s [Int]

  -- | Default method.
  -- Takes a list structure and an element, returns either the index of the
  -- first occurrence of the element in the list, or @Nothing@ if it is not in
  -- the list.
  -- Usually used as an infix function.
  mIndexOf :: l e s -> e -> ST s (Maybe Int)
  mIndexOf ml e = do
    indices <- mIndicesOf ml e
    return $ if null indices
      then Nothing
      else Just $ head indices

  -- | Default method.
  -- Takes a list structure and an element, returns @True@ if and only if the
  -- element is in the list.
  mContains :: l e s -> e -> ST s Bool
  mContains = (fmap isJust .) . mIndexOf

instance {-# OVERLAPPABLE #-} (Eq a, List l) => Eq (l a) where
  al == al' 
    = l == l' && and (map (liftM2 (==) (al `get`) (al' `get`)) [0..(l - 1)])
    where
      l  = size al
      l' = size al'

instance {-# OVERLAPPABLE #-} (Eq a, MList l) => Eq (l a s) where
  mal == mal'
    = unsafePerformIO $ unsafeSTToIO $ lazyToStrictST $ do
      l  <- mSize mal
      l' <- mSize mal'
      r  <- sequence $ 
        map (ap (liftM2 (==) . (mal `mGet`)) (mal' `mGet`)) [0..(l - 1)]
      return $ (l == l') && and r
