{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MMZKDS.FDQ (FDQ, balanceDeque) where

import          Data.Maybe (isJust, listToMaybe)

import          MMZKDS.Base (FDQ(..))
import          MMZKDS.Class.DS (DS(..), DSCons(..))
import          MMZKDS.Class.List (List(..))
import          MMZKDS.Class.Queue (Deque(..))
import          MMZKDS.Utilities (idFDQ, outOfBoundError)

instance Show a => Show (FDQ a) where
  show (FDQ _ frt _ end)
    = "Deque: " ++ show (frt ++ reverse end)


--------------------------------------------------------------------------------
-- List Instance
--------------------------------------------------------------------------------

instance List (FDQ a) a where
  delete :: FDQ a -> Int ->  (Maybe a, FDQ a)
  delete q@(FDQ fl frt el end) index
    | index < 0    = (Nothing, q)
    | index >= len = (Nothing, q)
    | index < fl   = let (f, e : f') = splitAt index frt
                     in  (Just e, balanceDeque $ FDQ (fl - 1) (f ++ f') el end)
    | otherwise    = let (r, e : r') = splitAt (len - index - 1) end
                     in  (Just e, balanceDeque $ FDQ fl frt (el - 1) (r ++ r'))
    where
      len = fl + el

  get :: FDQ a -> Int -> a
  get q@(FDQ fl frt el end) index
    | index >= len || index < 0 = outOfBoundError index
    | index < fl                = frt !! index
    | otherwise                 = end !! (len - index - 1)
    where
      len = fl + el

  indicesOf :: Eq a => FDQ a -> a -> [Int]
  indicesOf q@(FDQ fl frt _ end) e
    = fmap fst (filter ((== e) . snd) (zip [0..] frt)) ++ 
      fmap fst (filter ((== e) . snd) (zip [fl..] $ reverse end))

  insert :: FDQ a -> Int -> a ->  FDQ a
  insert q@(FDQ fl frt el end) index e
    | index > len = outOfBoundError index
    | index < 0   = outOfBoundError index
    | index <= fl = let (f, f') = splitAt index frt
                    in balanceDeque $ FDQ (fl + 1) (f ++ e : f') el end
    | otherwise   = let (r, r') = splitAt (len - index) end
                    in balanceDeque $ FDQ fl frt (el + 1) (r ++ e : r')
    where
      len = fl + el

  set :: FDQ a -> Int -> a -> FDQ a
  set q@(FDQ fl frt el end) index e
    | index >= len = outOfBoundError index
    | index < 0    = outOfBoundError index
    | index < fl   = let (f, _ : f') = splitAt index frt
                     in  balanceDeque $ FDQ fl (f ++ e : f') el end
    | otherwise    = let (r, _ : r') = splitAt (len - index - 1) end
                     in  balanceDeque $ FDQ fl frt el (r ++ e : r')
    where
      len = fl + el

  subList :: FDQ a -> Int -> Int -> FDQ a
  subList q inf sup
    = new $ take (sup - inf) $ drop inf $ toList q

  -- Overwritten default methods
  deleteRange :: FDQ a -> Int -> Int -> ([a], FDQ a)
  deleteRange q@(FDQ fl frt el end) inf sup
    | inf' >= fl = let (e1, er) = splitAt (size q - sup') end
                       (e2, e3) = splitAt diff er
                   in  ( reverse e2
                       , balanceDeque $ FDQ fl frt (el - diff) $ e1 ++ e3 )
    | sup' < fl  = let (f1, fr) = splitAt inf' frt
                       (f2, f3) = splitAt diff fr
                   in  (f2, balanceDeque $ FDQ (fl - diff) (f1 ++ f3) el end)
    | otherwise  = let (f1, m1) = splitAt inf' frt
                       (e1, m2) = splitAt (size q - sup') end
                   in ( m1 ++ reverse m2
                      , balanceDeque $ FDQ inf' f1 (size q - sup') e1 )
    where
      inf' = max 0 inf
      sup' = min (size q) sup
      diff = sup' - inf'

  -- Overwritten default methods
  lastIndexOf :: Eq a => FDQ a -> a -> Maybe Int
  lastIndexOf q@(FDQ fl frt _ end) e
    = listToMaybe $ 
      fmap fst (filter ((== e) . snd) (zip [sup, sup - 1..] end)) ++
      fmap fst (filter ((== e) . snd) (zip [fl - 1, fl - 2..] $ reverse frt))
      where
        sup = size q - 1
      

--------------------------------------------------------------------------------
-- Deque Instance
--------------------------------------------------------------------------------

instance Deque (FDQ a) a where
  dequeueFront :: FDQ a -> (Maybe a, FDQ a)
  dequeueFront = popFront

  dequeueEnd :: FDQ a -> (Maybe a, FDQ a)
  dequeueEnd = pop

  enqueueFront :: FDQ a -> a -> FDQ a
  enqueueFront = push

  enqueueEnd :: FDQ a -> a -> FDQ a
  enqueueEnd = append

  -- Overwritten default methods
  peekFront :: FDQ e -> Maybe e
  peekFront q
    | isNull q  = Nothing
    | otherwise = Just $ get q 0

 -- Overwritten default methods
  peekEnd :: FDQ e -> Maybe e
  peekEnd q
    | isNull q  = Nothing
    | otherwise = Just $ get q (size q - 1)


--------------------------------------------------------------------------------
-- DS & DSCons Instances
--------------------------------------------------------------------------------

instance DS (FDQ a) where
  clear :: FDQ a -> FDQ a
  clear = const (FDQ 0 [] 0 [])

  identifier :: FDQ a -> String
  identifier = const idFDQ

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


--------------------------------------------------------------------------------
-- Foldable Instance
--------------------------------------------------------------------------------

instance Foldable FDQ where
  foldr :: (a -> b -> b) -> b -> FDQ a -> b
  foldr f v = foldr f v . toList

  elem :: Eq a => a -> FDQ a -> Bool
  elem = (isJust .) . flip indexOf

  null :: FDQ a -> Bool
  null = isNull

  length :: FDQ a -> Int
  length = size
