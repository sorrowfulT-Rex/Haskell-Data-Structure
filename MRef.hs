{-# LANGUAGE FlexibleContexts #-}

module MRef where

import           Control.Monad.ST.Lazy
import           Data.Array.Base
import           Data.Array.ST

type MRef s a = STArray s Int a

newMRef :: a -> ST s (MRef s a)
newMRef = newArray (0, 0)

modifyMRef :: MRef s a -> (a -> a) -> ST s ()
modifyMRef ptr f = do
  v <- readMRef ptr
  writeMRef ptr $! f v

readMRef :: MRef s a -> ST s a
readMRef = flip readArray 0 

writeMRef :: MRef s a -> a -> ST s ()
writeMRef = flip writeArray 0
