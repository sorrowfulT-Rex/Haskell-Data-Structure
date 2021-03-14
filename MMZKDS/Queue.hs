{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.Queue where

import           MMZKDS.DS (DSCons(..))
import           MMZKDS.List as L (List(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- Queue Type Class
--------------------------------------------------------------------------------

class DSCons [e] (q e) => Queue q e where
  add :: e -> q e -> q e

  clear :: q e -> q e

  pop :: q e -> (Maybe e, q e)

class (Monad (m s), MDS (q e) s, MDSCons [e] (q e) s) => MQueue q e m s where
  mAdd :: e -> q e s -> m s ()

  mClear :: q e s -> m s ()

  mPop :: q e s -> m s (Maybe e)
