{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Queue (Queue(..), MQueue(..), Deque(..), MDeque(..)) where

import           Control.Monad (forM_)

import           MMZKDS.Class.DS (DS(..), DSCons(..))
import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))


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
-- MQueue Type Class
--------------------------------------------------------------------------------

-- | 'MQueue' is a type class for mutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- It should follow a queue logic (FIFO). More specifically, it should always 
-- add element to the rear and delete element from the front.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @mDequeue@, @mEnqueue@ and @mPeek@.
-- 
class (Monad (m s), MDS q m s, MDSCons [e] q m s)
  => MQueue q e m s | q -> e where
  -- | Removes the element at the front of the queue, returning the element.
  -- 
  mDequeue :: q s -> m s (Maybe e)

  -- | Adds an element to the rear.
  -- 
  mEnqueue :: e -> q s -> m s ()

  -- | Retrieves the element at the front, but not removing it.
  -- 
  mPeek :: q s -> m s (Maybe e)


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
-- MDeque Type Class
--------------------------------------------------------------------------------

-- | 'MDeque' is a type class for mutable deque data structures.
-- It provides methods of adding and removing element from both ends.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- A 'MDeque' is a 'MQueue' by default.
-- Minimal implementation requires @mDequeueFront@, @mDequeueEnd@, 
-- @mEnqueueFront@ and @mEnqueueEnd@.
class (Monad (m s), MDS q m s, MDSCons [e] q m s)
  => MDeque q e m s | q -> e where
  -- | Removes the element at the front of the queue, returning the element.
  -- 
  mDequeueFront :: q s -> m s (Maybe e)

  -- | Removes the element at the rear of the queue, returning the element.
  -- 
  mDequeueEnd :: q s -> m s (Maybe e)

  -- | Adds an element to the front.
  -- 
  mEnqueueFront :: e -> q s -> m s ()

  -- | Adds an element to the rear.
  -- 
  mEnqueueEnd :: e -> q s -> m s ()

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  -- 
  mPeekFront :: q s -> m s (Maybe e)
  mPeekFront q = do
    me <- mDequeueFront q
    forM_ me (`mEnqueueFront` q)
    return me

  -- | Default method.
  -- Retrieves the element at the rear, but not removing it.
  -- 
  mPeekEnd :: q s -> m s (Maybe e)
  mPeekEnd q = do
    me <- mDequeueEnd q
    forM_ me (`mEnqueueEnd` q)
    return me


--------------------------------------------------------------------------------
-- Deque -> Queue
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (DS (q a), DSCons [a] (q a), (Deque (q a) a)) 
  => Queue (q a) a where
  dequeue = dequeueFront
  enqueue = enqueueEnd


--------------------------------------------------------------------------------
-- MDeque -> MQueue
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} 
  (Monad (m s), MDS (q a) m s, MDSCons [a] (q a) m s, (MDeque (q a) a m s)) 
    => MQueue (q a) a m s where
  mDequeue = mDequeueFront
  mEnqueue = mEnqueueEnd
  mPeek    = mPeekFront
