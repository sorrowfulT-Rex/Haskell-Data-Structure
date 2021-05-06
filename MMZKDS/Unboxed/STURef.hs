{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MMZKDS.Unboxed.STURef where

import           Control.Monad.ST (ST)
import           Data.Array.ST
    (MArray, STUArray, newArray, readArray, writeArray)

-- | Type alias for the constraint @MArray (STUArray) s e (ST s)@, which forces
-- @e@ to be a primitive.
-- 
type STU e s = MArray (STUArray s) e (ST s)

-- | Type alias for the unboxed reference, which is implemented as an unboxed
-- @STUArray@ (with a length of 1).
-- 
type STURef s e = STUArray s Int e

-- | Allocates a new reference to the given value.
-- 
newSTURef :: MArray (STUArray s) a (ST s) => a -> ST s (STURef s a)
newSTURef = newArray (0, 0)

-- | Strictly modifies the value the reference refers to by the given function.
-- 
modifySTURef :: MArray (STUArray s) a (ST s) => STURef s a -> (a -> a) -> ST s ()
modifySTURef ptr f = do
  v <- readSTURef ptr
  writeSTURef ptr $! f v

-- | Reads the value the reference refers to.
-- 
readSTURef :: MArray (STUArray s) a (ST s) => STURef s a -> ST s a
readSTURef = flip readArray 0 

-- | Overwrites the value the reference refers to.
-- 
writeSTURef :: MArray (STUArray s) a (ST s) => STURef s a -> a -> ST s ()
writeSTURef = flip writeArray 0
