module ArrayBased where 

import           Data.Foldable

import           List

class List a => ArrayBased a where
  physicalSize :: a e -> Int
  newWithSize  :: Foldable f => Int -> f e -> a e
  resize       :: Int -> a e -> a e