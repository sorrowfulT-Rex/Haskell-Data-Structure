{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.PriorityQueue where

import           Control.Monad (when)
import           Data.Maybe (fromJust, isJust)

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- PriorityQueue Type Class
--------------------------------------------------------------------------------

-- | 'PriorityQueue' is a type class for immutable priority queue structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a priority queue logic, thus it should
-- have an defined order for its elements.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @add@ and @pop@.
-- Default method is @peek@.
-- 
-- Should satisfy the following:
-- When the queue is non-empty:
--  @ q ==== let (Just e, q') = pop q in add e q' @
--
class (DS (q e), DSCons [e] (q e)) => PriorityQueue q e where
  -- | Adds an element into the queue.
  add :: e -> q e -> q e

  -- | Removes the element at the front of the queue, returning a tuple of the
  -- element and the rest of the queue.
  pop :: q e -> (Maybe e, q e)

  -- | Default method.
  -- Retrieves the element at the front but not removing it.
  peek :: q e -> Maybe e
  peek = fst . MMZKDS.PriorityQueue.pop


--------------------------------------------------------------------------------
-- MPriorityQueue Type Class
--------------------------------------------------------------------------------

-- | 'MPriorityQueue' is a type class for mutable priority queue structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a priority queue logic, thus it should
-- have an defined order for its elements.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @mAdd@ and @mPop@.
-- Default method is @mPeek@.
-- 
-- Should satisfy the following:
-- When the queue is non-empty:
-- @ mPop mq ==== mPop mq >>= flip mAdd mq >> mPop mq @
--
class (Monad (m s), MDS (q e) m s, MDSCons [e] (q e) m s) 
  => MPriorityQueue q e m s where
  -- | Adds an element into the queue.
  mAdd :: e -> q e s -> m s ()

  -- | Removes the element at the front of the queue, returning the element.
  mPop :: q e s -> m s (Maybe e)

  -- | Default method.
  -- Retrieves the element at the front but not removing it.
  mPeek :: q e s -> m s (Maybe e)
  mPeek q = do
    me <- MMZKDS.PriorityQueue.mPop q
    when (isJust me) $ MMZKDS.PriorityQueue.mAdd (fromJust me) q
    return me
