module ArrayBased where 
import           Control.Monad.ST
import           Data.Foldable

import           List

class List a => ArrayBased a where
  newWithSize  :: Foldable f => Int -> f e -> a e
  physicalSize :: a e -> Int
  resize       :: Int -> a e -> a e

class MList a => MArrayBased a where
  mNewWithSize  :: Foldable f => Int -> f e -> ST s (a s e)
  mPhysicalSize :: a s e -> ST s Int
  mResize       :: Int -> a s e -> ST s (a s e)
