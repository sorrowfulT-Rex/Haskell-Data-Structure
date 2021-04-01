{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.FDQ (FDQ) where

import          MMZKDS.Base (FDQ(..))
import          MMZKDS.DS (DS(..), DSCons(..))
import          MMZKDS.Queue (Deque(..))

instance Show a => Show (FDQ a) where
  show (FDQ _ frt _ end)
    = "Deque: " ++ show (frt ++ reverse end)


--------------------------------------------------------------------------------
-- Deque Instance
--------------------------------------------------------------------------------

instance Deque FDQ a where
  dequeueFront :: FDQ a -> (Maybe a, FDQ a)
  dequeueFront (FDQ fl (e : frt) el end)
    = (Just e, balanceDeque $ FDQ (fl - 1) frt el end)
  dequeueFront (FDQ _ [] el (e : end))
    = (Just e, FDQ 0 [] (el - 1) end)
  dequeueFront q@(FDQ _ [] _ [])
    = (Nothing, q)

  dequeueEnd :: FDQ a -> (Maybe a, FDQ a)
  dequeueEnd (FDQ fl frt el (e : end))
    = (Just e, balanceDeque $ FDQ fl frt (el - 1) end)
  dequeueEnd (FDQ fl (e : frt) _ [])
    = (Just e, FDQ (fl - 1) frt 0 [])
  dequeueEnd q@(FDQ _ [] _ [])
    = (Nothing, q)

  enqueueFront :: a -> FDQ a -> FDQ a
  enqueueFront e (FDQ fl frt el end)
    = balanceDeque $ FDQ (fl + 1) (e : frt) el end

  enqueueEnd :: a -> FDQ a -> FDQ a
  enqueueEnd e (FDQ fl frt el end)
    = balanceDeque $ FDQ fl frt (el + 1) (e : end)


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance DS (FDQ a) where
  clear :: FDQ a -> FDQ a
  clear = const (FDQ 0 [] 0 [])

  size :: FDQ a -> Int
  size (FDQ fl _ el _)
    = fl + el

instance DSCons [a] (FDQ a) where
  finish :: FDQ a -> [a]
  finish (FDQ _ frt _ end)
    = frt ++ reverse end

  new :: [a] -> FDQ a
  new xs
    = let len        = length xs
          splitI     = len `div` 2
          (frt, end) = splitAt splitI xs
      in FDQ splitI frt (len - splitI) (reverse end)


--------------------------------------------------------------------------------
-- Deque-Specific Function
--------------------------------------------------------------------------------

-- | Balance the functional deque to achieve amortised O(1) operations.
-- 
balanceDeque :: FDQ a -> FDQ a
balanceDeque q@(FDQ fl frt el end)
  | 2 * fl < el = FDQ (fl + len) (frt ++ reverse endR) (el - len) endF
  | 2 * el < fl = FDQ (fl - len) frtF (el + len) (end ++ reverse frtR)
  | otherwise   = q
  where
    len          = abs (el - fl) `div` 2
    (frtF, frtR) = splitAt (fl - len) frt
    (endF, endR) = splitAt (el - len) end
