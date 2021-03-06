{-# LANGUAGE MultiParamTypeClasses #-}

module ArrayBased where 

import           Control.Monad.ST
import           Data.Foldable

import           List


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

arrayLengthOverflowError :: a
arrayLengthOverflowError = error $ "Length of array has overflowed!"


--------------------------------------------------------------------------------
-- ArrayBased Interface
--------------------------------------------------------------------------------

class ArrayBased a where
  deepClear    :: a e -> a e
  newWithSize  :: Foldable f => Int -> f e -> a e
  physicalSize :: a e -> Int
  resize       :: Int -> a e -> a e

class MArrayBased a where
  mDeepClear    :: a e s -> ST s ()
  mNewWithSize  :: Foldable f => Int -> f e -> ST s (a e s)
  mPhysicalSize :: a e s -> ST s Int
  mResize       :: Int -> a e s -> ST s (a e s)
  trueCopy      :: a e s -> ST s (a e s)
