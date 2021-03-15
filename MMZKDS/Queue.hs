{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Queue where

import           MMZKDS.DS (DSCons(..))
import           MMZKDS.List as L (List(..), MList(..))
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

instance (List l a, DSCons [a] (l a)) => Queue l a where
  add   = push
  clear = L.clear
  pop   = L.pop

instance (Monad (m s), MList l a m s, MDS (l a) s, MDSCons [a] (l a) s) 
  => MQueue l a m s where
  mAdd   = mPush
  mClear = L.mClear
  mPop   = L.mPop
