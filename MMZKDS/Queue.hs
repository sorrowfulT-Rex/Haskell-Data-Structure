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
-- it should always push to the front and pop from the rear.
-- It is expected that the type implements 'DS' and 'DSCons' with @[]@.
-- Minimal implementation requires @add@ and @pop@.
-- Default method is @peek@.
class (DS (q e), DSCons [e] (q e)) => Queue q e where
  -- | Adds an element to the front.
  add :: e -> q e -> q e

  -- | Removes the element from the rear of the queue, returning a tuple of the
  -- element and the rest of the queue.
  pop :: q e -> (Maybe e, q e)

  -- | Default method.
  -- Retrieves the element at the rear, but not removing it.
  peek :: q e -> Maybe e
  peek = fst . MMZKDS.Queue.pop

-- | 'MQueue' is a type class for mutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- The choice of the element being inserted or deleted is up to the 
-- implementation, but it should follow a queue logic (FIFO). More specifically,
-- it should always push on the front and pop on the rear.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @mAdd@, @mPeek@ and @mPop@.
class (Monad (m s), MDS (q e) m s, MDSCons [e] (q e) m s) 
  => MQueue q e m s where
  -- | Adds an element to the front.
  mAdd :: e -> q e s -> m s ()

  -- | Retrieves the element at the rear, but not removing it.
  mPeek :: q e s -> m s (Maybe e)

  -- | Removes the element at the rear of the queue, returning the element.
  mPop :: q e s -> m s (Maybe e)
