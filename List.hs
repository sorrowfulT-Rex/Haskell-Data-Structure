{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module List where
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Data.Bits
import           Data.Foldable
import           System.IO.Unsafe (unsafePerformIO)


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

outOfBoundError :: Int -> a
outOfBoundError i
  = error $ "Index " ++ show i ++ " is out of bound!"

initialSize :: Int -> Int
initialSize = expandedSize . shiftL 1 . ceiling . logBase 2 . fromIntegral

expandedSize :: Int -> Int
expandedSize = (1 +) . (`div` 2) . (3 *)


--------------------------------------------------------------------------------
-- List Interface
--------------------------------------------------------------------------------

class List l where
  add      :: Int -> e -> l e -> l e
  clear    :: l e -> l e
  get      :: l e -> Int -> e
  remove   :: Int -> l e -> (Maybe e, l e)
  set      :: l e -> Int -> e -> l e
  size     :: l e -> Int
  newList  :: Foldable f => f e -> l e 

  append :: e -> l e -> l e 
  append = flip (join (flip . add . size))

  isNull :: l e -> Bool
  isNull = (== 0) . size

  pop :: l e -> (Maybe e, l e)
  pop = remove 0

  popEnd :: l e -> (Maybe e, l e)
  popEnd = join (remove . (+ (-1)) . size)

  push :: e -> l e -> l e
  push = add 0

  update :: l e -> Int -> (e -> e) -> l e
  update = ap (ap . ((.) .) . set) ((flip id .) . get)

class MList l where
  mAdd     :: Int -> e -> l e s -> ST s ()
  mClear   :: l e s -> ST s ()
  mGet     :: l e s -> Int -> ST s e
  mRemove  :: Int -> l e s -> ST s (Maybe e)
  mSet     :: l e s -> Int -> e -> ST s ()
  mSize    :: l e s -> ST s Int
  mToList  :: l e s -> ST s [e]
  newMList :: Foldable f => f e -> ST s (l e s)

  mAppend :: e -> l e s -> ST s ()
  mAppend = liftM2 (>>=) mSize . flip . flip mAdd

  mIsNull :: l e s -> ST s Bool
  mIsNull = (>>= return . (== 0)) . mSize

  mPop :: l e s -> ST s (Maybe e)
  mPop = mRemove 0

  mPopEnd :: l e s -> ST s (Maybe e)
  mPopEnd mal = do
    l <- mSize mal
    mRemove (l - 1) mal

  mPush :: e -> l e s -> ST s ()
  mPush = mAdd 0

  mUpdate :: l e s -> Int -> (e -> e) -> ST s ()
  mUpdate mal index f = do
    v <- mGet mal index
    mSet mal index (f v)


--------------------------------------------------------------------------------
-- List With Eq
--------------------------------------------------------------------------------

class ListEq l e where
  indexOf  :: l e -> e -> Maybe Int
  contains :: l e -> e -> Bool

class MListEq l e s where
  mIndexOf  :: l e s -> e -> ST s (Maybe Int)
  mContains :: l e s -> e -> ST s Bool

instance {-# OVERLAPPABLE #-} (Eq a, List l) => Eq (l a) where
  al == al' 
    = l == l' && and (map (liftM2 (==) (al `get`) (al' `get`)) [0..(l - 1)])
    where
      l  = size al
      l' = size al'

instance {-# OVERLAPPABLE #-} (Eq a, MList l) => Eq (l a s) where
  mal == mal'
    = unsafePerformIO $ unsafeSTToIO $ do
      l  <- mSize mal
      l' <- mSize mal'
      r  <- sequence $ map (\i -> do
        v  <- mal `mGet` i
        v' <- mal' `mGet` i
        return $ v == v'
        ) [0..(l - 1)]
      return $ (l == l') && and r
