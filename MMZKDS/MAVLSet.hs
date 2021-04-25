{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MAVLSet (MAVLSet) where

import           Control.Monad.ST (ST)
import           Data.STRef (newSTRef, readSTRef, writeSTRef)

import           MMZKDS.Base (MAVLSet(..), MAVLTree(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.PriorityQueue (MPriorityQueue(..))
import           MMZKDS.Set as S (MSet(..))
import           MMZKDS.Unboxed.MURef (newMURef, readMURef)


--------------------------------------------------------------------------------
-- MSet Instance
--------------------------------------------------------------------------------

instance Ord a => MSet MAVLSet a ST s where
  -- mAdd = _
  -- mContains = _
  -- mFindAny = _
  -- mRemove = _


--------------------------------------------------------------------------------
-- MDS & MDSCons Instances
--------------------------------------------------------------------------------

instance MDS (MAVLSet a) ST s where
  clear :: MAVLSet a s -> ST s ()
  clear (MAVLSet tR) = writeSTRef tR MAVLEmpty

  copy :: MAVLSet a s -> ST s (MAVLSet a s)
  copy (MAVLSet tR) = do
    tree <- readSTRef tR
    let copy' t = case t of
          MAVLEmpty               -> newSTRef MAVLEmpty
          MAVLLeaf eR             -> do 
            e  <- readSTRef eR
            eR <- newSTRef e
            newSTRef $ MAVLLeaf eR
          MAVLNode sR dR lR eR rR -> do
            s  <- readMURef sR
            sR <- newMURef s
            d  <- readMURef dR
            dR <- newMURef d
            e  <- readSTRef eR
            eR <- newSTRef e
            l  <- readSTRef lR
            r  <- readSTRef rR
            lR <- copy' l
            rR <- copy' r
            newSTRef $ MAVLNode sR dR lR eR rR
    MAVLSet <$> copy' tree

  size :: MAVLSet a s -> ST s Int
  size (MAVLSet tR) = do
    tree <- readSTRef tR
    case tree of
      MAVLEmpty           -> return 0
      MAVLLeaf _          -> return 1
      MAVLNode sR _ _ _ _ -> readMURef sR
  

instance Ord a => MDSCons [a] (MAVLSet a) ST s where
  -- finish :: MAVLSet a s -> ST s [a]
  -- finish = 
  -- new = _
