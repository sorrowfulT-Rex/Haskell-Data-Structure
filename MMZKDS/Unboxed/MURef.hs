{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MMZKDS.Unboxed.MURef where

import           Control.Monad.ST
import           Data.Array.Base
import           Data.Array.ST
import           Data.Array.Unboxed

type MU e s    = MArray (STUArray s) e (ST s)
type MURef s e = STUArray s Int e

newMURef :: MArray (STUArray s) a (ST s) => a -> ST s (MURef s a)
newMURef = newArray (0, 0)

modifyMURef :: MArray (STUArray s) a (ST s) => MURef s a -> (a -> a) -> ST s ()
modifyMURef ptr f = do
  v <- readMURef ptr
  writeMURef ptr $! f v

readMURef :: MArray (STUArray s) a (ST s) => MURef s a -> ST s a
readMURef = flip readArray 0 

writeMURef :: MArray (STUArray s) a (ST s) => MURef s a -> a -> ST s ()
writeMURef = flip writeArray 0
