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
  newList  :: Foldable f => f e -> l e 
  remove   :: Int -> l e -> (Maybe e, l e)
  set      :: l e -> Int -> e -> l e
  size     :: l e -> Int

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

class List l => ListEq l e where
  indicesOf :: l e -> e -> [Int]

  indexOf :: l e -> e -> Maybe Int
  indexOf l e
    | notFound  = Nothing
    | otherwise = Just $ head indices
    where
      notFound = null indices
      indices  = indicesOf l e

  contains :: l e -> e -> Bool
  contains = (isJust .) . indexOf

class MList l => MListEq l e s where
  mIndicesOf :: l e s -> e -> ST s [Int]

  mIndexOf :: l e s -> e -> ST s (Maybe Int)
  mIndexOf ml e = do
    indices <- mIndicesOf ml e
    return $ if null indices
      then Nothing
      else Just $ head indices

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
