module MDT where

import           Control.Monad
import           Control.Monad.ST

class MDT d where
  copy :: d s e -> ST s (d s e)
