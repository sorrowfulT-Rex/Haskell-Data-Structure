{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module List where
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe (unsafeSTToIO)
import           Data.Foldable
import           System.IO.Unsafe (unsafePerformIO)

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
  mAdd     :: Int -> e -> l s e -> ST s ()
  mClear   :: l s e -> ST s ()
  mGet     :: l s e -> Int -> ST s e
  mRemove  :: Int -> l s e -> ST s (Maybe e)
  mSet     :: l s e -> Int -> e -> ST s ()
  mSize    :: l s e -> ST s Int
  mToList  :: l s e -> ST s [e]
  newMList :: Foldable f => f e -> ST s (l s e)

  mAppend :: e -> l s e -> ST s ()
  mAppend = liftM2 (>>=) mSize . flip . flip mAdd

  mIsNull :: l s e -> ST s Bool
  mIsNull = (>>= return . (== 0)) . mSize

  mPop :: l s e -> ST s (Maybe e)
  mPop = mRemove 0

  mPopEnd :: l s e -> ST s (Maybe e)
  mPopEnd mal = do
    l <- mSize mal
    mRemove (l - 1) mal

  mPush :: e -> l s e -> ST s ()
  mPush = mAdd 0

  mUpdate :: l s e -> Int -> (e -> e) -> ST s ()
  mUpdate mal index f = do
    v <- mGet mal index
    mSet mal index (f v)

class ListEq l e where
  isElem :: e -> l e -> Bool

class MListEq l s e where
  mIsElem :: e -> l s e -> ST s Bool

instance {-# OVERLAPPABLE #-} (Eq a, List l) => Eq (l a) where
  al == al' 
    = l == l' && and (map (liftM2 (==) (al `get`) (al' `get`)) [0..(l - 1)])
    where
      l  = size al
      l' = size al'

instance {-# OVERLAPPABLE #-} (Eq a, MList l) => Eq (l s a) where
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
