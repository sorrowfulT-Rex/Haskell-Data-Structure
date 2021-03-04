module List where
import           Control.Monad
import           Data.Foldable

class List l where
  add     :: Int -> e -> l e -> l e
  pop     :: l e -> (Maybe e, l e)
  popEnd  :: l e -> (Maybe e, l e)
  remove  :: Int -> e -> le -> (Maybe e, l e)
  size    :: l e -> Int
  newList :: Foldable f => f e -> l e 

  append :: e -> l e -> l e 
  append = flip (join (flip . add . size))

  isNull :: l e -> Bool
  isNull = (== 0) . size

  push :: e -> l e -> l e
  push = add 0
