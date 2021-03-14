{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.DS where

import           GHC.Exts (IsList(..))

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
