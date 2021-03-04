module List where
import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable

class List l where
  add     :: Int -> e -> l e -> l e
  pop     :: l e -> (Maybe e, l e)
  popEnd  :: l e -> (Maybe e, l e)
  remove  :: Int -> e -> l e -> (Maybe e, l e)
  size    :: l e -> Int
  newList :: Foldable f => f e -> l e 

  append :: e -> l e -> l e 
  append = flip (join (flip . add . size))

  isNull :: l e -> Bool
  isNull = (== 0) . size

  push :: e -> l e -> l e
  push = add 0

class MList l where
  mAdd     :: Int -> e -> l s e -> ST s (l s e)
  mPop     :: l s e -> ST s (Maybe e)
  mPopEnd  :: l s e -> ST s (Maybe e)
  mRemove  :: Int -> e -> l s e -> ST s (Maybe e)
  mSize    :: l s e -> ST s Int
  newMList :: Foldable f => f e -> ST s (l s e)

  mAppend :: e -> l s e -> ST s (l s e)
  mAppend = liftM2 (>>=) mSize . flip . flip mAdd

  mIsNull :: l s e -> ST s Bool
  mIsNull = (>>= return . (== 0)) . mSize

  mPush :: e -> l s e -> ST s (l s e)
  mPush = mAdd 0
