{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module MMZKDS.Class.MPriorityQueue (MPriorityQueue(..)) where

import           Control.Monad (forM_)

import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- MPriorityQueue Type Class
--------------------------------------------------------------------------------

-- | 'MPriorityQueue' is a type class for mutable priority queue structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a priority queue logic, thus it should
-- have an defined order for its elements.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @add@ and @pop@.
-- Default method is @peek@.
-- 
-- Should satisfy the following:
-- When the queue is non-empty:
-- @ mPop mq ==== mPop mq >>= flip mAdd mq >> mPop mq @
--
class (Monad (m s), MDS q m s, MDSCons [e] q m s)
  => MPriorityQueue q e m s | q -> e where
  -- | Adds an element into the queue.
  add :: q s -> e -> m s ()

  -- | Removes the element at the front of the queue, returning the element.
  pop :: q s -> m s (Maybe e)

  -- | Default method.
  -- Retrieves the element at the front but not removing it.
  peek :: q s -> m s (Maybe e)
  peek q = do
    me <- pop q
    forM_ me $ add q
    return me
