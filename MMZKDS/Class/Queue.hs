{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Class.Queue (Queue(..), Deque(..)) where

import           MMZKDS.Class.DS (DS(..), DSCons(..))


--------------------------------------------------------------------------------
-- Queue Type Class
--------------------------------------------------------------------------------

-- | 'Queue' is a type class for immutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- It should follow a queue logic (FIFO). More specifically, it should always 
-- add element to the rear and delete element from the front.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @dequeue@ and @enqueue@.
-- Default method is @peek@.
-- 
class (DS q, DSCons [e] q) => Queue q e | q -> e where
  -- | Removes the element from the front of the queue, returning a tuple of the
  -- element and the rest of the queue.
  -- 
  dequeue :: q -> (Maybe e, q)

  -- | Adds an element to the rear.
  -- 
  enqueue :: e -> q -> q

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  -- 
  peek :: q -> Maybe e
  peek = fst . dequeue


--------------------------------------------------------------------------------
-- Deque Type Class
--------------------------------------------------------------------------------

-- | 'Deque' is a type class for immutable deque data structures.
-- It provides methods of adding and removing element from both ends.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- A 'Deque' is a 'Queue' by default.
-- Minimal implementation requires @dequeueFront@, @dequeueEnd@, @enqueueFront@ 
-- and @enqueueEnd@.
-- Default method is @peekFront@ and @peekEnd@.
-- 
class (DS q, DSCons [e] q) => Deque q e | q -> e where
  -- | Removes the element from the front of the queue, returning a tuple of the
  -- element and the rest of the queue.
  -- 
  dequeueFront :: q -> (Maybe e, q)

  -- | Removes the element from the rear of the queue, returning a tuple of the
  -- element and the rest of the queue.
  -- 
  dequeueEnd :: q -> (Maybe e, q)

  -- | Adds an element to the front.
  -- 
  enqueueFront :: e -> q -> q

  -- | Adds an element to the rear.
  -- 
  enqueueEnd :: e -> q -> q

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  -- 
  peekFront :: q -> Maybe e
  peekFront = fst . dequeueFront

  -- | Default method.
  -- Retrieves the element at the rear, but not removing it.
  -- 
  peekEnd :: q -> Maybe e
  peekEnd = fst . dequeueEnd


--------------------------------------------------------------------------------
-- Deque -> Queue
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (DS (q a), DSCons [a] (q a), (Deque (q a) a)) 
  => Queue (q a) a where
  dequeue = dequeueFront
  enqueue = enqueueEnd
