module List where

import           Data.Foldable

class List l where
  append  :: e -> l e -> l e 
  add     :: Int -> e -> l e -> l e
  pop     :: l e -> (Maybe e, l e)
  popEnd  :: l e -> (Maybe e, l e)
  push    :: e -> l e -> l e 
  remove  :: Int -> e -> le -> (Maybe e, l e)
  size    :: l e -> Int
  newList :: Foldable f => f e -> l e 

  isNull  :: l e -> Bool
  isNull = (== 0) . size
