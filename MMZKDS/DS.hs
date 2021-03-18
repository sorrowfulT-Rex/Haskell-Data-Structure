{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.DS where

import           GHC.Exts (IsList(..))

-- | The 'DS' class is a type class for mutable data structures living in the 
-- ST monad.
-- It provides a method to clear all elements and to check the size.
-- Minimum implementation reqires @clear@, @size@.
-- Default method is @isNull@.
--
class DS d where
  -- | Makes the data structure empty, i.e. remove all elements.
  -- Note that it is not guaranteed that any element is physically removed from
  -- the structure; the method may simply render all elements inaccessible.
  --
  clear :: d -> d

  -- | Returns the size (length) of the list structure.
  --
  size :: d -> Int

  -- | Default method.
  -- Returns @True@ if and only if the list structure is empty.
  --
  isNull :: d -> Bool
  isNull = (== 0) . size

-- | The 'DSCons' class defines how to initialise the immutable data structure
-- from or to a potentially different immutable data structure.
-- It provides a @finish@ method and a @new@ method.
--
class DSCons a d where
  -- | Turn the mutable data structure to the given immutable data structure
  finish :: d -> a

  -- | Create a new mutable data structure from the given immutable data 
  -- structure.
  --
  new :: a -> d

instance DSCons [a] (d a) => IsList (d a) where
  type Item (d a) = a
  fromList        = new
  toList          = finish
