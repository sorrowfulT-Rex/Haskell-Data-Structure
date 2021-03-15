{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MDS where

import           Control.Monad.ST (ST(..))

-- | The 'MDS' class is a type class for mutable data structures living in the 
-- (strict) ST monad.
-- It provides a single @copy@ method.
--
class Monad (m s) => MDS d m s where
  -- | The 'copy' method returns a new mutable data structure containing the
  -- same data as the argument provides.
  -- Note that this copy is "shallow" by nature, in other words, if the data
  -- structure contains pointers such as @STRef@, it is expected to copy the
  -- reference only.
  --
  copy :: d s -> m s (d s)

-- | The 'MDSCons' class defines how to initialise the mutable data structure
-- from or to a potentially different immutable data structure.
-- It provides a @finish@ method and a @new@ method.
-- For example, assume there is a mutable data structure by the name of 
-- 'Foo a s', then @instance [a] (Foo a) s@ is used to define how to make a new 
-- instance of 'Foo' from a list.
--
class Monad (m s) => MDSCons a d m s where
    -- | Turn the mutable data structure to the given immutable data structure
  finish :: d s -> m s a

  -- | Create a new mutable data structure from the given immutable data 
  -- structure.
  --
  new :: a -> m s (d s)
