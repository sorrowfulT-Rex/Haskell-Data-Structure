{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MAVLSet (MAVLSet) where

import           Control.Monad (forM_)
import           Control.Monad.ST (ST, runST)
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
  mAdd :: a -> MAVLSet a s -> ST s ()
  mAdd e (MAVLSet tR) = add' tR
    where
      add' tR = do
        tree <- readSTRef tR
        case tree of
          MAVLEmpty               -> newSTRef e >>= writeSTRef tR . MAVLLeaf
          MAVLLeaf eR'            -> do
            e' <- readSTRef eR'
            if e == e'
              then writeSTRef eR' e
              else do
                s2 <- newMURef 2
                d2 <- newMURef 2
                eR <- newSTRef e
                ep <- newSTRef MAVLEmpty
                sb <- newSTRef (MAVLLeaf eR)
                writeSTRef tR $ if e < e'
                  then MAVLNode s2 d2 sb eR' ep
                  else MAVLNode s2 d2 ep eR' sb
          MAVLNode sR dR lR eR' rR -> do
            e' <- readSTRef eR'
            if e == e'
              then writeSTRef eR' e
              else if e < e'
                then add' lR
                else add' rR

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
  finish :: MAVLSet a s -> ST s [a]
  finish (MAVLSet tR) = finish' [] tR
    where
      finish' stack tR = do
        tree <- readSTRef tR
        case tree of
          MAVLEmpty           -> if null stack
            then return []
            else let MAVLNode _ _ _ eR rR = head stack
                 in  readSTRef eR >>= (<$> finish' (tail stack) rR) . (:)
          MAVLLeaf eR         -> do
            e <- readSTRef eR
            if null stack
              then return [e]
              else let MAVLNode _ _ _ eR rR = head stack
                   in  fmap (e :) $ 
                         readSTRef eR >>= (<$> finish' (tail stack) rR) . (:)
          MAVLNode _ _ lR _ _ -> finish' (tree : stack) lR

  new :: [a] -> ST s (MAVLSet a s)
  new xs = do
    res <- MAVLSet <$> newSTRef MAVLEmpty
    forM_ xs $ flip S.mAdd res
    return res

foo :: [Int]
foo = runST $ do
  set   <- new [1, 4, 2, 8, 5, 7, 6 :: Int] :: ST s (MAVLSet Int s)
  tes   <- MAVLSet <$> newSTRef MAVLEmpty
  S.mAdd 3 tes
  -- set `mUnion` tes
  finish set
