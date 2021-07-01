{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.Class.MUnorderedList where

import           Control.Monad (void, when)
import           Data.Maybe (isJust, fromJust)

import           MMZKDS.Class.MDS as MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- MSet Type Class
--------------------------------------------------------------------------------

-- | 'MUnorderedList' is a type class for mutable list structures where the 
-- elements are not ordered by index, but by its intrinsic order.
-- It is similar to a (mutable) list that can have duplicates.
-- It is expected that the elements are instances of 'Ord'.  
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[]@.
-- Minimal implementation requires @add@, @contains@, @getNthMin@, and @remove@.
-- Default methods include @count@, @difference@, @difference'@, @dropAny@, 
-- @findAny@, @getMax@, @getNthMax@, @getMin@, @intersection@, @intersection'@, 
-- @newUnorderedList@, @toList@, @union@ and @union'@.
--
class (Monad (m s), MDS l m s, MDSCons [e] l m s) 
  => MUnorderedList l e m s | l -> e where
  -- | Adds an element into the list.
  --
  add :: l s -> e -> m s ()

  -- | Tests if the element is in the list.
  --
  contains :: l s -> e -> m s (Maybe (Int, Int))

  -- | Gets the n-th minimum element in the list.
  --
  getNthMin :: l s -> Int -> m s (Maybe e)

  -- | Removes an element into the list.
  -- Returns the removed element (or Nothing, if element is not in the list), and 
  -- deletes the element from the list.
  --
  remove :: l s -> e -> m s (Maybe e)

  -- | Default method.
  -- Counts the number of occurrences of an element in the list.
  --
  count :: l s -> e -> m s Int
  count l e = do
    mb <- contains l e
    return $ case mb of
      Just (inf, sup) -> sup - inf + 1
      Nothing         -> 0

  -- | Default method.
  -- Computes the difference of two sets, and update it to the first list.
  --
  difference :: forall c1. MDSCons [e] c1 m s => l s -> c1 s -> m s ()
  difference
    = flip ((>>=) . (MDS.finish :: c1 s -> m s [e])) . mapM_ . remove

  -- | Default method.
  -- Computes the difference of two sets, and update it to the first list.
  -- The second list is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  difference' :: l s -> l s -> m s ()
  difference' = difference

  dropAny :: l s -> m s (Maybe e)
  dropAny s = do
    me <- getMin s
    when (isJust me) $ void $ remove s (fromJust me)
    return me

  -- | Default method.
  -- Gets the maximum element in the list.
  --
  getMax :: l s -> m s (Maybe e)
  getMax = flip getNthMax 1

  -- | Default method.
  -- Gets the nth maximum element in the list.
  --
  getNthMax :: l s -> Int -> m s (Maybe e)
  getNthMax l i = do
    s <- size l
    getNthMin l (s - i + 1)

  -- | Default method.
  -- Gets the minimum element in the list.
  --
  getMin :: l s -> m s (Maybe e)
  getMin = flip getNthMin 1

  -- | Default method.
  -- Computes the intersection of two sets, and update it to the first list.
  --
  intersection :: forall c1. MDSCons [e] c1 m s => l s -> c1 s -> m s ()
  intersection d ds = do
    d' <- MDS.copy d
    difference d' ds
    difference d d'

  -- | Default method.
  -- Computes the intersection of two sets, and update it to the first list.
  -- The second list is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  intersection' :: l s -> l s -> m s ()
  intersection' = intersection

  -- | Default method.
  -- Returns a new list from @[]@.
  -- 
  newUnorderedList :: [e] -> m s (l s)
  newUnorderedList = MDS.new

  -- | Default method.
  -- Return the list representation of the list.
  -- 
  toList :: l s -> m s [e]
  toList = MDS.finish

  -- | Default method.
  -- Computes the union of two sets, and update it to the first list.
  --
  union :: forall c1. MDSCons [e] c1 m s => l s -> c1 s -> m s ()
  union = flip ((>>=) . (MDS.finish :: c1 s -> m s [e])) . mapM_ . add

  -- | Default method.
  -- Computes the union of two sets, and update it to the first list.
  -- The second list is of the same type as the first one.
  -- For some instances of "Set" this may have a more efficient implementation.
  --
  union' :: l s -> l s -> m s ()
  union' = union
