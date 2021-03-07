{-# LANGUAGE MultiParamTypeClasses #-}

module ArrayBased where 

import           Control.Monad.ST.Lazy (ST(..))
import           Data.Foldable (toList)


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Utility Function. 
-- Returns an error indicating the length of the array has exceeds the limit.
-- For arrays indexed on @Int@, however, this is not going to happen in practice
-- since it requires more than 1024 PiB memory.
-- 
arrayLengthOverflowError :: a
arrayLengthOverflowError = error "Length of array has overflowed!"


--------------------------------------------------------------------------------
-- ArrayBased Interface
--------------------------------------------------------------------------------

-- | 'ArrayBased' is a type class for immutable array-based data structure.
-- It provides methods to allocate new arrays for length adjustment.
-- Instances of 'ArrayBased' is required to implement 'Foldable'.
-- Minimal implementation requires @deepClear@, @newWithSize@ and 
-- @physicalSize@.
--
class Foldable a => ArrayBased a where
  -- | Returns a new structure that is truly empty; in other words, all elements
  -- are physically removed from the structure.
  --
  deepClear :: a e -> a e

  -- | Takes an @Int@ as length and an instance of 'Foldable', creates a new 
  -- structure containing the elements in the 'Foldable' and the representing
  -- array has at least the length specified by the argument.
  --
  newWithSize  :: Foldable f => Int -> f e -> a e

  -- | Returns the physical size of the structure, in other words, the length
  -- of the representing array.
  --
  physicalSize :: a e -> Int

  -- | Optional method.
  -- | Takes an Int as length and a structure, returns a structure containing
  -- the same elements but with at least the length specified by the argument.
  --
  resize :: Int -> a e -> a e
  resize = (. toList) . newWithSize

-- | 'MArrayBased' is a type class for mutable @STArray@-based data structure.
-- It provides methods to allocate new arrays for length adjustment, and to copy
-- the structure that retains it's physical size.
-- Instances of 'ArrayBased' is required to implement 'Foldable'.
-- Minimal implementation requires @mDeepClear@, @newMWithSize@, 
-- @mPhysicalSize@, @mResize@ and @trueCopy@.
--
class MArrayBased a where
  -- | Truly empties the structure; in other words, all elements are physically 
  -- removed from the structure.
  --
  mDeepClear :: a e s -> ST s ()

  -- | Takes an @Int@ as length and an instance of 'Foldable', creates a new 
  -- structure containing the elements in the 'Foldable' and the representing
  -- array has at least the length specified by the argument.
  -- 
  newMWithSize :: Foldable f => Int -> f e -> ST s (a e s)

  -- | Returns the physical size of the structure, in other words, the length
  -- of the representing array.
  -- 
  mPhysicalSize :: a e s -> ST s Int

  -- | Takes an @Int@ as length and a structure, modifies the structure such that
  -- it has at least the length specified by the argument.
  --
  mResize :: Int -> a e s -> ST s (a e s)

  -- | Create a new mutable data structure from the given mutable data 
  -- structure, retaining the physical size.
  --
  trueCopy :: a e s -> ST s (a e s)
