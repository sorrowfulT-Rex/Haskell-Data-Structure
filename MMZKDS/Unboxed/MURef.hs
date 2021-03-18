{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MMZKDS.Unboxed.MURef where

import           Control.Monad.ST (ST)
import           Data.Array.ST
    (MArray(newArray), STUArray, readArray, writeArray)

-- | Type alias for the constraint MArray (STUArray) s e (ST s), which forces
-- e to be a primitive.
type MU e s = MArray (STUArray s) e (ST s)

-- | Type alias for the unboxed reference, which is implemented as an unboxed
-- @STUArray (with a length of 1).
type MURef s e = STUArray s Int e

-- | Allocates a new reference to the given value.
newMURef :: MArray (STUArray s) a (ST s) => a -> ST s (MURef s a)
newMURef = newArray (0, 0)

-- | Strictly modifies the value the reference refers to by the given function.
modifyMURef :: MArray (STUArray s) a (ST s) => MURef s a -> (a -> a) -> ST s ()
modifyMURef ptr f = do
  v <- readMURef ptr
  writeMURef ptr $! f v

-- | Reads the value the reference refers to.
readMURef :: MArray (STUArray s) a (ST s) => MURef s a -> ST s a
readMURef = flip readArray 0 

-- | Overwrites the value the reference refers to.
writeMURef :: MArray (STUArray s) a (ST s) => MURef s a -> a -> ST s ()
writeMURef = flip writeArray 0
