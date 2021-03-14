{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.ArrayBased where 

import           Control.Monad (forM_)
import           Data.Foldable (toList)

import           MMZKDS.DS (DSCons(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- ArrayBased Type Class
--------------------------------------------------------------------------------

-- | 'ArrayBased' is a type class for immutable array-based data structure.
-- It provides methods to allocate new arrays for length adjustment.
-- Minimal implementation requires @deepClear@, @newWithSize@, @physicalSize@
-- and @resize@.
--
class DSCons [e] (a e) => ArrayBased a e where
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

  -- | Takes an Int as length and a structure, returns a structure containing
  -- the same elements but with at least the length specified by the argument.
  --
  resize :: Int -> a e -> a e

-- | 'MArrayBased' is a type class for mutable @STArray@-based data structure.
-- It provides methods to allocate new arrays for length adjustment, and to copy
-- the structure that retains it's physical size.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[e]@.
-- Instances of 'ArrayBased' is required to implement 'Foldable'.
-- Minimal implementation requires @mDeepClear@, @mNewWithSize@, 
-- @mPhysicalSize@, @mResize@ and @trueCopy@.
--
class (Monad (m s), MDS (a e) s, MDSCons [e] (a e) s) 
  => MArrayBased a e m s where
  -- | Truly empties the structure; in other words, all elements are physically 
  -- removed from the structure.
  --
  mDeepClear :: a e s -> m s ()

  -- | Takes an @Int@ as length and an instance of 'Foldable', creates a new 
  -- structure containing the elements in the 'Foldable' and the representing
  -- array has at least the length specified by the argument.
  -- 
  mNewWithSize :: Foldable f => Int -> f e -> m s (a e s)

  -- | Returns the physical size of the structure, in other words, the length
  -- of the representing array.
  -- 
  mPhysicalSize :: a e s -> m s Int

  -- | Takes an @Int@ as length and a structure, modifies the structure such that
  -- it has at least the length specified by the argument.
  --
  mResize :: Int -> a e s -> m s (a e s)

  -- | Create a new mutable data structure from the given mutable data 
  -- structure, retaining the physical size.
  --
  trueCopy :: a e s -> m s (a e s)
