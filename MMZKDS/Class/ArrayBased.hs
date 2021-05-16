{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module MMZKDS.Class.ArrayBased (ArrayBased(..)) where 

import           MMZKDS.Class.DS (DS(..), DSCons(..))


--------------------------------------------------------------------------------
-- ArrayBased Type Class
--------------------------------------------------------------------------------

-- | 'ArrayBased' is a type class for immutable array-based data structure.
-- It provides methods to allocate new arrays for length adjustment.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @deepClear@, @newWithSize@, @physicalSize@
-- and @resize@.
--
class (DS a, DSCons [e] a) => ArrayBased a e | a -> e where
  -- | Returns a new structure that is truly empty; in other words, all elements
  -- are physically removed from the structure.
  --
  deepClear :: a -> a

  -- | Takes an @Int@ as length and an instance of 'Foldable', creates a new 
  -- structure containing the elements in the 'Foldable' and the representing
  -- array has at least the length specified by the argument.
  --
  newWithSize  :: Foldable f => Int -> f e -> a

  -- | Returns the physical size of the structure, in other words, the length
  -- of the representing array.
  --
  physicalSize :: a -> Int

  -- | Takes an Int as length and a structure, returns a structure containing
  -- the same elements but with at least the length specified by the argument.
  --
  resize :: Int -> a -> a
