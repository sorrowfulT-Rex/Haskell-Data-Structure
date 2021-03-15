{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Queue where

import           Data.Maybe (fromJust, isNothing)

import           MMZKDS.DS (DSCons(..))
import           MMZKDS.List as L 
  (List(push, clear, pop), MList(mPush, mClear, mPop))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- Queue Type Class
--------------------------------------------------------------------------------

-- | 'Queue' is a type class for immutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a queue or priority queue logic.
-- It is expected that the type implements 'DSCons' with @[]@.
-- Minimal implementation requires @add@ and @pop@.
-- Default method is @peek@.
class DSCons [e] (q e) => Queue q e where
  -- | Adds an element into the queue.
  add :: e -> q e -> q e

  clear :: q e -> q e

  -- | Removes the element at the "front" of the queue, returning a tuple of the
  -- element and the rest of the queue.
  pop :: q e -> (Maybe e, q e)

  -- | Default method.
  -- Retrieves the element at the "front", but not removing it.
  peek :: q e -> Maybe e
  peek = fst . MMZKDS.Queue.pop

-- | 'MQueue' is a type class for mutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a queue or priority queue logic.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @mAdd@ and @mPop@.
-- Default method is @mPeek@.
class (Monad (m s), MDS (q e) m s, MDSCons [e] (q e) m s) 
  => MQueue q e m s where
  -- | Adds an element into the queue.
  mAdd :: e -> q e s -> m s ()

  mClear :: q e s -> m s ()

  -- | Removes the element at the "front" of the queue, returning the element.
  mPop :: q e s -> m s (Maybe e)

  -- | Default method.
  -- Retrieves the element at the "front", but not removing it.
  mPeek :: q e s -> m s (Maybe e)
  mPeek mq = do
    e <- MMZKDS.Queue.mPop mq
    if isNothing e
      then return e
      else mAdd (fromJust e) mq >> return e

instance (List l a, DSCons [a] (l a)) => Queue l a where
  add   = push
  clear = L.clear
  pop   = L.pop

instance (Monad (m s), MList l a m s, MDS (l a) m s, MDSCons [a] (l a) m s) 
  => MQueue l a m s where
  mAdd   = mPush
  mClear = L.mClear
  mPop   = L.mPop
