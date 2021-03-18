{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MHeapQ where

import           Control.Monad.ST (ST)
import           Data.Array (Array)
import           Data.Array.ST (STArray, freeze, thaw)
import           Data.Foldable (toList)
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.PriorityQueue (MPriorityQueue(..))
import           MMZKDS.Unboxed.MURef (MURef, readMURef)

data MHeapQ o e s = MHeapQ {
  mHeapF :: e -> o,
  mHeapS :: MURef s Int,
  mHeapA :: STArray s Int e
  }


instance MPriorityQueue (MHeapQ o) e ST s where
  mAdd = undefined
  mPop = undefined


instance MDS (MHeapQ o a) ST s where
  clear = undefined
  copy = undefined
  size = undefined

instance MDSCons [a] (MHeapQ o a) ST s where
  finish :: MHeapQ o a s -> ST s [a]
  finish mh = do
    al <- (freeze :: STArray s Int a -> ST s (Array Int a)) $ mHeapA mh 
    return $ toList al

  new :: [a] -> ST s (MHeapQ o a s)
  new = undefined
