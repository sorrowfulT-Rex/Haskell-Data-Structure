{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.MAVLSet (MAVLSet, avlSetFreeze, avlSetThaw) where

import           Control.Monad (ap, forM_, liftM2, liftM5)
import           Control.Monad.ST (ST, runST)
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import           MMZKDS.Base (AVLSet(..), MAVLSet(..), MAVLTree(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))
import           MMZKDS.PriorityQueue (MPriorityQueue(..))
import           MMZKDS.Set as S (MSet(..))
import           MMZKDS.Unboxed.MURef
  (modifyMURef, newMURef, readMURef, writeMURef)


--------------------------------------------------------------------------------
-- Freeze & Thaw
--------------------------------------------------------------------------------

-- | Make an immutable @AVLSet@ from a @MAVLSet@.
-- 
avlSetFreeze :: MAVLSet a s -> ST s (AVLSet a)
avlSetFreeze (MAVLSet tR) = freeze' tR
  where
    freeze' tR = do
      tree <- readSTRef tR
      case tree of
        MAVLEmpty               -> return AVLEmpty
        MAVLLeaf eR             -> AVLLeaf <$> readSTRef eR
        MAVLNode sR dR lR eR rR -> liftM5 AVLNode 
                                          (readMURef sR) 
                                          (readMURef dR)
                                          (freeze' lR) 
                                          (readSTRef eR) 
                                          (freeze' rR)

-- | Make a mutable @MAVLSet@ from a @AVLSet@.
--
avlSetThaw :: AVLSet a -> ST s (MAVLSet a s)
avlSetThaw set = MAVLSet <$> (thaw' set >>= newSTRef)
  where
    thaw' AVLEmpty            
      = return MAVLEmpty
    thaw' (AVLLeaf e)         
      = MAVLLeaf <$> newSTRef e
    thaw' (AVLNode s d l e r) 
      = liftM5 MAVLNode (newMURef s)
                        (newMURef d)
                        (thaw' l >>= newSTRef)
                        (newSTRef e)
                        (thaw' r >>= newSTRef)


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
              else do
                if e < e'
                  then add' lR
                  else add' rR
                modifyMURef sR succ
                liftM2 max (depthBTN lR) (depthBTN rR) >>= writeMURef dR . succ

  -- mContains = _

  mFindAny :: MAVLSet a s -> ST s (Maybe a)
  mFindAny (MAVLSet tR) = do
    tree <- readSTRef tR
    case tree of
      MAVLEmpty           -> return Nothing
      MAVLLeaf eR         -> Just <$> readSTRef eR
      MAVLNode _ _ _ eR _ -> Just <$> readSTRef eR
      
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
          MAVLLeaf eR             -> readSTRef eR >>= newSTRef >>= 
                                     newSTRef . MAVLLeaf
          MAVLNode sR dR lR eR rR -> liftM5 MAVLNode
                                            (readMURef sR >>= newMURef)
                                            (readMURef dR >>= newMURef)
                                            (readSTRef lR >>= copy')
                                            (readSTRef eR >>= newSTRef)
                                            (readSTRef rR >>= copy') 
                                     >>= newSTRef
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
  new = (newSTRef MAVLEmpty >>=) . (. MAVLSet) . (`ap` return) . 
    ((>>) .) . (. flip S.mAdd) . forM_


--------------------------------------------------------------------------------
-- AVL-Tree Specific Function
--------------------------------------------------------------------------------

-- | Utility Function.
-- Returns the depth of the the AVL-tree.
-- 
depthBTN :: Ord a => STRef s (MAVLTree a s) -> ST s Int
depthBTN tR = do
  tree <- readSTRef tR
  case tree of
    MAVLNode _ dR _ _ _ -> readMURef dR
    MAVLLeaf _          -> return 1
    _                   -> return 0
