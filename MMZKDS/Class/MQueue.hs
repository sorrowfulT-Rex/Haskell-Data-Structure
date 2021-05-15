{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZKDS.Class.MQueue (MQueue(..), MDeque(..)) where

import           Control.Monad (forM_)

import           MMZKDS.Class.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- MQueue Type Class
--------------------------------------------------------------------------------

-- | 'MQueue' is a type class for mutable queue data structures.
-- It provides methods of adding and removing element from the queue.
-- It should follow a queue logic (FIFO). More specifically, it should always 
-- add element to the rear and delete element from the front.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @dequeue@, @enqueue@ and @peek@.
-- 
class (Monad (m s), MDS q m s, MDSCons [e] q m s)
  => MQueue q e m s | q -> e where
  -- | Removes the element at the front of the queue, returning the element.
  -- 
  dequeue :: q s -> m s (Maybe e)

  -- | Adds an element to the rear.
  -- 
  enqueue :: q s -> e -> m s ()

  -- | Retrieves the element at the front, but not removing it.
  -- 
  peek :: q s -> m s (Maybe e)


--------------------------------------------------------------------------------
-- MDeque Type Class
--------------------------------------------------------------------------------

-- | 'MDeque' is a type class for mutable deque data structures.
-- It provides methods of adding and removing element from both ends.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- A 'MDeque' is a 'MQueue' by default.
-- Minimal implementation requires @dequeueFront@, @dequeueEnd@, 
-- @enqueueFront@ and @enqueueEnd@.
class (Monad (m s), MDS q m s, MDSCons [e] q m s)
  => MDeque q e m s | q -> e where
  -- | Removes the element at the front of the queue, returning the element.
  -- 
  dequeueFront :: q s -> m s (Maybe e)

  -- | Removes the element at the rear of the queue, returning the element.
  -- 
  dequeueEnd :: q s -> m s (Maybe e)

  -- | Adds an element to the front.
  -- 
  enqueueFront :: q s -> e -> m s ()

  -- | Adds an element to the rear.
  -- 
  enqueueEnd :: q s -> e -> m s ()

  -- | Default method.
  -- Retrieves the element at the front, but not removing it.
  -- 
  peekFront :: q s -> m s (Maybe e)
  peekFront q = do
    me <- dequeueFront q
    forM_ me (q `enqueueFront`)
    return me

  -- | Default method.
  -- Retrieves the element at the rear, but not removing it.
  -- 
  peekEnd :: q s -> m s (Maybe e)
  peekEnd q = do
    me <- dequeueEnd q
    forM_ me (q `enqueueEnd`)
    return me


--------------------------------------------------------------------------------
-- MDeque -> MQueue
--------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} 
  (Monad (m s), MDS (q a) m s, MDSCons [a] (q a) m s, (MDeque (q a) a m s)) 
    => MQueue (q a) a m s where
  dequeue = dequeueFront
  enqueue = enqueueEnd
  peek    = peekFront
