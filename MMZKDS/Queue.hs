{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Queue (Queue(..), MQueue(..), Deque(..), MDeque(..)) where

import           Control.Monad (forM_)

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


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
class (DS (q e), DSCons [e] (q e)) => Queue q e where
  -- | Removes the element from the front of the queue, returning a tuple of the
  -- element and the rest of the queue.
  -- 
  dequeue :: q e -> (Maybe e, q e)

  -- | Adds an element to the rear.
  -- 
  enqueue :: e -> q e -> q e

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  -- 
  peek :: q e -> Maybe e
  peek = fst . dequeue


--------------------------------------------------------------------------------
-- MQueue Type Class
--------------------------------------------------------------------------------

-- | 'MQueue' is a type class for mutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- It should follow a queue logic (FIFO). More specifically, it should always 
-- add element to the rear and delete element from the front.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @mDequeue@, @mEnqueue@ and @mPeek@.
-- 
class (Monad (m s), MDS (q e) m s, MDSCons [e] (q e) m s)
  => MQueue q e m s where
  -- | Removes the element at the front of the queue, returning the element.
  -- 
  mDequeue :: q e s -> m s (Maybe e)

  -- | Adds an element to the rear.
  -- 
  mEnqueue :: e -> q e s -> m s ()

  -- | Retrieves the element at the front, but not removing it.
  -- 
  mPeek :: q e s -> m s (Maybe e)


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
class (DS (q e), DSCons [e] (q e)) => Deque q e where
  -- | Removes the element from the front of the queue, returning a tuple of the
  -- element and the rest of the queue.
  -- 
  dequeueFront :: q e -> (Maybe e, q e)

  -- | Removes the element from the rear of the queue, returning a tuple of the
  -- element and the rest of the queue.
  -- 
  dequeueEnd :: q e -> (Maybe e, q e)

  -- | Adds an element to the front.
  -- 
  enqueueFront :: e -> q e -> q e

  -- | Adds an element to the rear.
  -- 
  enqueueEnd :: e -> q e -> q e

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  -- 
  peekFront :: q e -> Maybe e
  peekFront = fst . dequeueFront

  -- | Default method.
  -- Retrieves the element at the rear, but not removing it.
  -- 
  peekEnd :: q e -> Maybe e
  peekEnd = fst . dequeueEnd


--------------------------------------------------------------------------------
-- MDeque Type Class
--------------------------------------------------------------------------------

-- | 'MDeque' is a type class for mutable deque data structures.
-- It provides methods of adding and removing element from both ends.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- A 'MDeque' is a 'MQueue' by default.
-- Minimal implementation requires @mDequeueFront@, @mDequeueEnd@, 
-- @mEnqueueFront@ and @mEnqueueEnd@.
class (Monad (m s), MDS (q e) m s, MDSCons [e] (q e) m s)
  => MDeque q e m s where
  -- | Removes the element at the front of the queue, returning the element.
  -- 
  mDequeueFront :: q e s -> m s (Maybe e)

  -- | Removes the element at the rear of the queue, returning the element.
  -- 
  mDequeueEnd :: q e s -> m s (Maybe e)

  -- | Adds an element to the front.
  -- 
  mEnqueueFront :: e -> q e s -> m s ()

  -- | Adds an element to the rear.
  -- 
  mEnqueueEnd :: e -> q e s -> m s ()

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  -- 
  mPeekFront :: q e s -> m s (Maybe e)
  mPeekFront q = do
    me <- mDequeueFront q
    forM_ me (`mEnqueueFront` q)
    return me

  -- | Default method.
  -- Retrieves the element at the rear, but not removing it.
  -- 
  mPeekEnd :: q e s -> m s (Maybe e)
  mPeekEnd q = do
    me <- mDequeueEnd q
    forM_ me (`mEnqueueEnd` q)
    return me


--------------------------------------------------------------------------------
-- Deque -> Queue
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (DS (q a), DSCons [a] (q a), (Deque q a)) 
  => Queue q a where
  dequeue = dequeueFront
  enqueue = enqueueEnd


--------------------------------------------------------------------------------
-- MDeque -> MQueue
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} 
  (Monad (m s), MDS (q a) m s, MDSCons [a] (q a) m s, (MDeque q a m s)) 
    => MQueue q a m s where
  mDequeue = mDequeueFront
  mEnqueue = mEnqueueEnd
  mPeek = mPeekFront
