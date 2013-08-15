-- |
-- Module:     Control.Wire.FRP.Analyze
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Analyze
    ( -- * Linear graphs
      lAvg,
      lGraph,
      lGraphN,

      -- * Staircase graphs
      sGraph,
      sGraphN,

      -- * Peaks
      highPeak,
      lowPeak,
      highPeakBy,
      lowPeakBy,

      -- * Debug
      framerate
    )
    where

import qualified Control.Wire.Timeline as Tl
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Wire.State
import Control.Wire.Wire
import Data.List
import Data.Map (Map)
import Data.Monoid
import Prelude hiding ((.), id)


-- | Current framerate.

framerate ::
    (Fractional b, HasTime t s, Monad m, Monoid e)
    => Wire s e m a b
framerate =
    mkPure_ $ \ds _ ->
        let dt = dtime ds in
        if dt <= 0
          then Left mempty
          else Right $! recip (realToFrac dt)


-- | High peak.
--
-- * Depends: now.

highPeak :: (Monad m, Monoid s, Ord a) => Wire s e m a a
highPeak = highPeakBy compare


-- | High peak with respect to the given comparison function.
--
-- * Depends: now.

highPeakBy :: (Monad m, Monoid s) => (a -> a -> Ordering) -> Wire s e m a a
highPeakBy = peakBy GT


-- | Calculate the average of the signal over the given interval (from
-- now) using the given number of data points evenly distributed over
-- the interval.
--
-- Linear interpolation can be slow.  If you don't need it, you can use
-- the staircase variant 'sAvg'.
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

lAvg ::
    (Fractional a, Fractional t, HasTime t s, Monad m, Monoid e)
    => t    -- ^ Interval size.
    -> Int  -- ^ Number of data points to use.
    -> Wire s e m a a
lAvg int n = (/ fromIntegral n) . foldl' (+) 0 <$> lGraphN int n


-- | Produce a linearly interpolated graph for the given points in time,
-- where the magnitudes of the points are distances from /now/.
--
-- Linear interpolation can be slow.  If you don't need it, you can use
-- the faster staircase variant 'sGraph'.
--
-- Example: @lGraph [0, 1, 2]@ will output the interpolated inputs at
-- /now/, one second before now and two seconds before now.
--
-- * Complexity: O(s) space, O(n * log s) time, where s = number of
--   samples in the interval, n = number of requested data points.
--
-- * Depends: now.

lGraph ::
    (Fractional a, Fractional t, HasTime t s, Monad m, Monoid e)
    => [t]  -- ^ Data points to produce.
    -> Wire s e m a [a]
lGraph qts =
    mkPure $ \ds x ->
        let t = dtime ds in
        (Right (x <$ qts), loop t (Tl.singleton t x))

    where
    earliest = maximum (map abs qts)

    loop t' tl' =
        mkPure $ \ds x ->
            let t  = t' + dtime ds
                tl = Tl.linCut (t - earliest) (Tl.insert t x tl')
                ps = map (\qt -> Tl.linLookup (t - abs qt) tl) qts
            in (Right ps, loop t tl)


-- | Graph the given interval from now with the given number of evenly
-- distributed points in time.  Convenience interface to 'lGraph'.
--
-- Linear interpolation can be slow.  If you don't need it, you can use
-- the faster staircase variant 'sGraphN'.
--
-- * Complexity: O(s) space, O(n * log s) time, where s = number of
--   samples in the interval, n = number of requested data points.
--
-- * Depends: now.

lGraphN ::
    (Fractional a, Fractional t, HasTime t s, Monad m, Monoid e)
    => t    -- ^ Interval to graph from now.
    -> Int  -- ^ Number of data points to produce.
    -> Wire s e m a [a]
lGraphN int n
    | int <= 0 = error "lGraphN: Non-positive interval"
    | n <= 0   = error "lGraphN: Non-positive number of data points"
lGraphN int n =
    let n1 = n - 1 in
    lGraph (map (\qt -> realToFrac int * fromIntegral qt / fromIntegral n1) [0..n1])


-- | Low peak.
--
-- * Depends: now.

lowPeak :: (Monad m, Monoid s, Ord a) => Wire s e m a a
lowPeak = lowPeakBy compare


-- | Low peak with respect to the given comparison function.
--
-- * Depends: now.

lowPeakBy :: (Monad m, Monoid s) => (a -> a -> Ordering) -> Wire s e m a a
lowPeakBy = peakBy LT


-- | Given peak with respect to the given comparison function.

peakBy ::
    (Monad m, Monoid s)
    => Ordering
    -> (a -> a -> Ordering)
    -> Wire s e m a a
peakBy o comp = mkPure $ \_ x -> (Right x, loop x)
    where
    loop x' =
        mkPure $ \_ x ->
            Right &&& loop $
            if comp x x' == o then x else x'


-- | Calculate the average of the signal over the given interval (from
-- now).
--
-- This views the incoming signal as a staircase.  If you need linear
-- interpolation instead, see 'lAvg'.
--
-- * Complexity: O(s) space and time wrt number of samples in the
--   interval.
--
-- * Depends: now.

-- sAvg ::
--     (HasTime t s, Monad m, Monoid e)
--     => t  -- ^ Interval.
--     -> Wire s e m a a
-- sAvg int | int <= 0 = error "sAvg: Non-positive interval"
-- sAvg int = loop 0 Seq.empty
--     where
--     loop t samp' =
--         mkPure $ \ds x ->
--             let samp = dropSamples t (samp' |> (ds, x))


-- | Produce a staircase graph for the given points in time, where the
-- magnitudes of the points are distances from /now/.  The keys of the
-- resulting map are just the given points in time.
--
-- See also 'lGraph', if you need linear interpolation.
--
-- Example: @sGraph [0, 1, 2]@ will output the inputs at /now/, one
-- second before now and two seconds before now.
--
-- * Complexity: O(s) space wrt number of samples in the interval, O(n)
--   time wrt number of data points.
--
-- * Depends: now.
--
-- * Inhibits: When no samples were received for the duration of the
--   given time interval.

sGraph ::
    (HasTime t s, Monad m, Monoid e)
    => [t]  -- ^ Data points to produce.
    -> Wire s e m a (Map t a)
sGraph qts = loop 0 M.empty
    where
    earliest = maximum (map abs qts)

    interpolate rt qt samp =
        case M.lookupLE qt samp of
          Just (_, x) -> Right $! M.singleton rt x
          _           -> Left mempty

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
-- distributed points in time.  Convenience interface to 'sGraph'.
--
-- See also 'lGraphN', if you need linear interpolation.
--
-- * Complexity: O(s) space wrt number of samples in the interval, O(n)
--   time wrt number of data points.
--
-- * Depends: now.
--
-- * Inhibits: When no samples were received for the duration of the
--   given time interval.

sGraphN ::
    (Fractional t, HasTime t s, Monad m, Monoid e)
    => t    -- ^ Interval to graph from now.
    -> Int  -- ^ Number of data points to produce.
    -> Wire s e m a (Map t a)
sGraphN int n
    | int <= 0 = error "sGraphN: Non-positive interval"
    | n <= 0   = error "sGraphN: Non-positive number of data points"
sGraphN int n =
    let n1 = n - 1 in
    sGraph (map (\qt -> realToFrac int * fromIntegral qt / fromIntegral n1) [0..n1])
