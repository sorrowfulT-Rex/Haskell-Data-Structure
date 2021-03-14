{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.Queue where

import           MMZKDS.DS (DSCons(..))
import           MMZKDS.List as L (List(..))


--------------------------------------------------------------------------------
-- Queue Type Class
--------------------------------------------------------------------------------

class DSCons [e] (q e) => Queue q e where
  add :: e -> q e -> q e

  clear :: q e -> q e

  pop :: q e -> (Maybe e, q e)
