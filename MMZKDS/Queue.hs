{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Queue where

import           Control.Monad (when)
import           Data.Maybe (fromJust, isJust)

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.List as L 
  (List(push, pop), MList(mAppend, mPush, mPop))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- Queue Type Class
--------------------------------------------------------------------------------

-- | 'Queue' is a type class for immutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a queue or priority queue logic.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @add@ and @pop@.
-- Default method is @peek@.
class (DS (q e), DSCons [e] (q e)) => Queue q e where
  -- | Adds an element into the queue.
  add :: e -> q e -> q e

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
-- Minimal implementation requires @mAdd@, @mPeek@ and @mPop@.
class (Monad (m s), MDS (q e) m s, MDSCons [e] (q e) m s) 
  => MQueue q e m s where
  -- | Adds an element into the queue.
  mAdd :: e -> q e s -> m s ()

  -- Retrieves the element at the "front", but not removing it.
  mPeek :: q e s -> m s (Maybe e)

  -- | Removes the element at the "front" of the queue, returning the element.
  mPop :: q e s -> m s (Maybe e)


--------------------------------------------------------------------------------
-- List -> Queue
--------------------------------------------------------------------------------

-- A 'List' is by default a 'Queue' that adds in the front and pops in the rear.

instance (List l a, DS (l a), DSCons [a] (l a)) => Queue l a where
  add   = push
  pop   = L.pop

instance (Monad (m s), MList l a m s, MDS (l a) m s, MDSCons [a] (l a) m s) 
  => MQueue l a m s where
  mAdd = mPush
  
  mPeek m = do
    e <- L.mPop m
    when (isJust e) $ mAppend (fromJust e) m
    return e

  mPop = L.mPop
