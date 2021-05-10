{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module MMZKDS.ArrayBased (ArrayBased(..), MArrayBased(..)) where 

import           Control.Monad (forM_)

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- ArrayBased Type Class
--------------------------------------------------------------------------------

-- | 'ArrayBased' is a type class for immutable array-based data structure.
-- It provides methods to allocate new arrays for length adjustment.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @deepClear@, @newWithSize@, @physicalSize@
-- and @resize@.
--
class (DS a, DSCons [e] a e) => ArrayBased a e | a -> e where
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


--------------------------------------------------------------------------------
-- MArrayBased Type Class
--------------------------------------------------------------------------------

-- | 'MArrayBased' is a type class for mutable @STArray@-based data structure.
-- It provides methods to allocate new arrays for length adjustment, and to copy
-- the structure that retains it's physical size.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Instances of 'ArrayBased' is required to implement 'Foldable'.
-- Minimal implementation requires @mDeepClear@, @mNewWithSize@, 
-- @mPhysicalSize@, @mResize@ and @trueCopy@.
--
class (Monad (m s), MDS a m s, MDSCons [e] a e m s) 
  => MArrayBased a e m s | a -> e where
  -- | Truly empties the structure; in other words, all elements are physically 
  -- removed from the structure.
  --
  mDeepClear :: a s -> m s ()

  -- | Takes an @Int@ as length and an instance of 'Foldable', creates a new 
  -- structure containing the elements in the 'Foldable' and the representing
  -- array has at least the length specified by the argument.
  -- 
  mNewWithSize :: Foldable f => Int -> f e -> m s (a s)

  -- | Returns the physical size of the structure, in other words, the length
  -- of the representing array.
  -- 
  mPhysicalSize :: a s -> m s Int

  -- | Takes an @Int@ as length and a structure, modifies the structure such that
  -- it has at least the length specified by the argument.
  --
  mResize :: Int -> a s -> m s ()

  -- | Create a new mutable data structure from the given mutable data 
  -- structure, retaining the physical size.
  --
  trueCopy :: a s -> m s (a s)
