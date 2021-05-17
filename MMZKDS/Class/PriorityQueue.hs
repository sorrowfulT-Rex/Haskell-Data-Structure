{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module MMZKDS.Class.PriorityQueue (PriorityQueue(..)) where

import           MMZKDS.Class.DS (DS(..), DSCons(..))


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
class (DS q, DSCons [e] q) => PriorityQueue q e | q -> e where
  -- | Adds an element into the queue.
  add :: q -> e -> q

  -- | Removes the element at the front of the queue, returning a tuple of the
  -- element and the rest of the queue.
  pop :: q -> (Maybe e, q)

  -- | Default method.
  -- Retrieves the element at the front but not removing it.
  peek :: q -> Maybe e
  peek = fst . pop
