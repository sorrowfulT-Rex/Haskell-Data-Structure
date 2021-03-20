{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Queue where

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- Queue Type Class
--------------------------------------------------------------------------------

-- | 'Queue' is a type class for immutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a queue logic (FIFO). More specifically,
-- it should always add element to the rear and delete element from the front.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @dequeue@ and @enqueue@.
-- Default method is @peek@.
class (DS (q e), DSCons [e] (q e)) => Queue q e where
  -- | Removes the element from the front of the queue, returning a tuple of the
  -- element and the rest of the queue.
  dequeue :: q e -> (Maybe e, q e)

  -- | Adds an element to the rear.
  enqueue :: e -> q e -> q e

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  peek :: q e -> Maybe e
  peek = fst . MMZKDS.Queue.dequeue

-- | 'MQueue' is a type class for mutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a queue logic (FIFO). More specifically,
-- it should always add element to the rear and delete element from the front.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @mAdd@, @mPeek@ and @mPop@.
class (Monad (m s), MDS (q e) m s, MDSCons [e] (q e) m s) 
  => MQueue q e m s where
  -- | Removes the element at the front of the queue, returning the element.
  mDequeue :: q e s -> m s (Maybe e)

  -- | Adds an element to the rear.
  mEnqueue :: e -> q e s -> m s ()

  -- | Retrieves the element at the front, but not removing it.
  mPeek :: q e s -> m s (Maybe e)
