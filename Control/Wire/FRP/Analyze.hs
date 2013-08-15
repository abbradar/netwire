-- |
-- Module:     Control.Wire.FRP.Analyze
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Analyze
    ( -- * Average
      avg,
      avgFps,

      -- * Graph
      graph,
      graphN,

      -- * Peaks
      highPeak,
      lowPeak,
      highPeakBy,
      lowPeakBy,

      -- * Debug
      framerate
    )
    where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Wire.State
import Control.Wire.Wire
import Data.Map (Map)
import Data.Monoid
import Prelude hiding ((.), id)


-- | Calculate the average of the signal over the given interval (from
-- now) using the given number of data points evenly distributed over
-- the interval.
--
-- Example: @avg 1 100@
--
-- * Complexity: O(s) space wrt number of samples in the interval, O(n)
--   time wrt number of data points.
--
-- * Depends: now.
--
-- * Inhibits: When no samples were received for the duration of the
--   given time interval.

avg ::
    (Fractional a, Fractional t, HasTime t ds, Monad m, Monoid e)
    => t    -- ^ Interval size.
    -> Int  -- ^ Number of data points to use.
    -> Wire ds e m a a
avg int n = (/ fromIntegral n) . F.foldl' (+) 0 <$> graphN int n


-- | Calculate the average framerate (frames per second) over the given
-- sampling interval with the given number of data points evenly
-- distributed over the interval.
--
-- Example: @avgFps 10 100@
--
-- * Complexity: O(s) space wrt number of samples in the interval, O(n)
--   time wrt number of data points.
--
-- * Inhibits: When no samples were received for the duration of the
--   given time interval.

avgFps ::
    (Fractional b, Fractional t, HasTime t ds, Monad m, Monoid e)
    => t    -- ^ Interval size.
    -> Int  -- ^ Number of data points to use.
    -> Wire ds e m a b
avgFps int n = avg int n . framerate


-- | Current framerate.

framerate ::
    (Fractional b, HasTime t ds, Monad m, Monoid e)
    => Wire ds e m a b
framerate =
    mkPure_ $ \ds _ ->
        let dt = dtime ds in
        if dt <= 0
          then Left mempty
          else Right $! recip (realToFrac dt)


-- | Produce a linearly interpolated graph for the given points in time,
-- where the magnitudes of the points are distances from /now/.  The
-- keys of the resulting map are just the given points in time.
--
-- Example: @graph [0, 1, 2]@ will output the interpolated inputs at
-- /now/, one second before now and two seconds before now.
--
-- * Complexity: O(s) space wrt number of samples in the interval, O(n)
--   time wrt number of data points.
--
-- * Depends: now.
--
-- * Inhibits: When no samples were received for the duration of the
--   given time interval.

graph ::
    (Fractional a, Fractional t, HasTime t ds, Monad m, Monoid e)
    => [t]  -- ^ Data points to produce.
    -> Wire ds e m a (Map t a)
graph qts = loop 0 M.empty
    where
    earliest = maximum (map abs qts)

    interpolate rt qt samp =
        case (M.lookupLE qt samp, M.lookupGE qt samp) of
          (Just (t1, x1), Just (t2, x2)) ->
              let i = t2 - t1
                  f | i == 0     = 0.5
                    | otherwise = realToFrac ((qt - t1) / i)
                  x = x1*(1 - f) + x2*f
              in Right $! M.singleton rt x
          (Just (_, x), Nothing) -> Right $! M.singleton rt x
          (Nothing, Just (_, x)) -> Right $! M.singleton rt x
          _                      -> Left mempty

    loop t' samp'' =
        mkPure $ \ds x ->
            let t      = t' + dtime ds
                samp'  = M.insert t x samp''
                t0'    = t - earliest
                t0     = maybe t0' fst (M.lookupLE t0' samp')
                samp   = M.filterWithKey (\st _ -> st >= t0) samp'
                points = foldM (\m qt ->
                                    M.union m <$>
                                    interpolate qt (t - abs qt) samp)
                               M.empty qts
            in points `seq` (points, loop t samp)


-- | Graph the given interval from now with the given number of evenly
-- distributed points in time.  Convenience interface to 'graph'.
--
-- * Complexity: O(s) space wrt number of samples in the interval, O(n)
--   time wrt number of data points.
--
-- * Depends: now.
--
-- * Inhibits: When no samples were received for the duration of the
--   given time interval.

graphN ::
    (Fractional a, Fractional t, HasTime t ds, Monad m, Monoid e)
    => t    -- ^ Interval to graph from now.
    -> Int  -- ^ Number of data points to produce.
    -> Wire ds e m a (Map t a)
graphN int n
    | int <= 0 = error "graphN: Non-positive interval"
    | n <= 0   = error "graphN: Non-positive number of data points"
graphN int n =
    let n1 = n - 1 in
    graph (map (\qt -> realToFrac int * fromIntegral qt / fromIntegral n1) [0..n1])


-- | High peak.
--
-- * Depends: now.

highPeak :: (Monad m, Monoid ds, Ord a) => Wire ds e m a a
highPeak = highPeakBy compare


-- | High peak with respect to the given comparison function.
--
-- * Depends: now.

highPeakBy :: (Monad m, Monoid ds) => (a -> a -> Ordering) -> Wire ds e m a a
highPeakBy = peakBy GT


-- | Low peak.
--
-- * Depends: now.

lowPeak :: (Monad m, Monoid ds, Ord a) => Wire ds e m a a
lowPeak = lowPeakBy compare


-- | Low peak with respect to the given comparison function.
--
-- * Depends: now.

lowPeakBy :: (Monad m, Monoid ds) => (a -> a -> Ordering) -> Wire ds e m a a
lowPeakBy = peakBy LT


-- | Given peak with respect to the given comparison function.

peakBy ::
    (Monad m, Monoid ds)
    => Ordering
    -> (a -> a -> Ordering)
    -> Wire ds e m a a
peakBy o comp = mkPure $ \_ x -> (Right x, loop x)
    where
    loop x' =
        mkPure $ \_ x ->
            Right &&& loop $
            if comp x x' == o then x else x'
