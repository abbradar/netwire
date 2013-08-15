-- |
-- Module:     Control.Wire.Timeline
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Timeline
    ( -- * Time lines for statistics wires
      Timeline,

      -- * Constructing time lines
      insert,
      singleton,
      union,

      -- * Linear sampling
      linCut,
      linLookup
    )
    where

import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Data
import Data.Map.Strict (Map)


-- | A time line is a non-empty set of samples together with time
-- information.

newtype Timeline t a = Timeline (Map t a)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Functor (Timeline t) where
    fmap f (Timeline m) = Timeline (M.map f m)


-- | Insert the given data point.

insert :: (Ord t) => t -> a -> Timeline t a -> Timeline t a
insert t x (Timeline m) = Timeline (M.insert t x m)


-- | Cut the timeline at the given point in time @t@, such that all
-- samples up to but not including @t@ are forgotten.  The most recent
-- sample before @t@ is moved and interpolated accordingly.

linCut :: (Fractional a, Fractional t, Real t) => t -> Timeline t a -> Timeline t a
linCut t tl@(Timeline m) =
    Timeline $
    case M.splitLookup t m of
      (_, Just x, mr) -> M.insert t x mr
      (_, _, mr)      -> M.insert t (linLookup t tl) mr


-- | Look up with linear sampling.

linLookup :: (Fractional a, Fractional t, Real t) => t -> Timeline t a -> a
linLookup t (Timeline m) =
    case M.splitLookup t m of
      (_, Just x, _) -> x
      (ml, _, mr)    ->
          case (fst <$> M.maxViewWithKey ml, fst <$> M.minViewWithKey mr) of
            (Just (t1, x1), Just (t2, x2)) ->
                let f = realToFrac ((t - t1) / (t2 - t1))
                in x1*(1 - f) + x2*f
            (Just (_, x), _) -> x
            (_, Just (_, x)) -> x
            _                -> error "linLookup: BUG: querying empty Timeline"


-- | Singleton timeline with the given point.

singleton :: t -> a -> Timeline t a
singleton t = Timeline . M.singleton t


-- | Union of two time lines.  Right-biased.

union :: (Ord t) => Timeline t a -> Timeline t a -> Timeline t a
union (Timeline m1) (Timeline m2) = Timeline (M.union m2 m1)
