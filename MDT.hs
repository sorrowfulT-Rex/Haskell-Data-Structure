{-# LANGUAGE MultiParamTypeClasses #-}

module MDT where

import           Control.Monad
import           Control.Monad.ST

class MDT d s where
  copy :: d s -> ST s (d s)

class MDTCons a d s where
  new :: a -> ST s (d s)
