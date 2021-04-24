{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.UnionFind where

import           Control.Monad (liftM2)
import           Data.Maybe (fromJust, fromMaybe)

import           MMZKDS.DS (DS(..), DSCons(..))
import           MMZKDS.MDS (MDS(..), MDSCons(..))


--------------------------------------------------------------------------------
-- UnionFind Type Class
--------------------------------------------------------------------------------

-- | 'UnionFind' is a type class for immutable union-find structures.
-- It provides methods of finding the representative of a given equivalance
-- class in the structure, and merging two classes into one.
-- It is expected that the type implements 'DS' and 'DSCons' with @[[]]@.
-- Minimal implementation requires @find@ and @union@.
-- Default method is @isEquiv@ and @(!)@.
-- 
class (DS (q e), DSCons [[e]] (q e)) => UnionFind q e where
  -- | Find the representative of a give element. If the element is not in the
  -- union-find, return @Nothing@.
  -- 
  find :: q e -> e -> Maybe e

  -- | Merge the two equivalence classes of the given elements. If at least one
  -- of them is not in the union-find, or if they represent the same class, do
  -- nothing.
  -- 
  union :: e -> e -> q e -> q e

  -- | Default method.
  -- | Test if two elements are in the same class.
  --
  isEquiv :: Eq e => e -> e -> q e -> Bool
  isEquiv a b uf 
    = fromMaybe False $ liftM2 (==) (uf `find` a) (uf `find` b)

  -- | Default method.
  -- Find the representative of a give element. If the element is not in the
  -- union-find, it gives an error.
  infix 1 !
  (!) :: q e -> e -> e
  (!) = (fromJust .) . find


--------------------------------------------------------------------------------
-- MUnionFind Type Class
--------------------------------------------------------------------------------

-- | 'MUnionFind' is a type class for mutable union-find structures.
-- It provides methods of finding the representative of a given equivalance
-- class in the structure, and merging two classes into one.
-- It is expected that the type implements 'MDS' and 'MDSCons' with @[[]]@.
-- Minimal implementation requires @mFind@ and @mUnion@.
-- Default method is @mIsEquiv@ and @(~!)@.
-- 
class (Monad (m s), MDS (q e) m s, MDSCons [[e]] (q e) m s) 
  => MUnionFind q e m s where
  -- | Find the representative of a give element. If the element is not in the
  -- union-find, return @Nothing@.
  -- 
  mFind :: q e s -> e -> m s (Maybe e)

  -- | Merge the two equivalence classes of the given elements. If at least one
  -- of them is not in the union-find, or if they represent the same class, do
  -- nothing.
  -- 
  mUnion :: e -> e -> q e s -> m s ()

  -- | Default method.
  -- | Test if two elements are in the same class.
  --
  mEsEquiv :: Eq e => e -> e -> q e s -> m s Bool
  mEsEquiv a b muf = do
    aRep <- muf `mFind` a
    bRep <- muf `mFind` b
    return $ fromMaybe False $ liftM2 (==) aRep bRep

  -- | Default method.
  -- Find the representative of a give element. If the element is not in the
  -- union-find, it gives an error.
  infix 1 ~!
  (~!) :: q e s -> e -> m s e
  (~!) = (fmap fromJust .) . mFind


--------------------------------------------------------------------------------
-- Construct from [] as Singleton
--------------------------------------------------------------------------------

instance UnionFind q e => DSCons [e] (q e) where
  finish = concat . (MMZKDS.DS.finish :: q e -> [[e]])

  new = MMZKDS.DS.new . map (: [])

instance MUnionFind q e m s => MDSCons [e] (q e) m s where
  finish = fmap concat . (MMZKDS.MDS.finish :: q e s -> m s [[e]])

  new = MMZKDS.MDS.new . map (: [])
