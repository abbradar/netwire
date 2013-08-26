-- |
-- Module:     Control.Wire.FRP.Core
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Core
    ( -- * Time
      time,
      timeF,
      timeFrom,

      -- * Unfold
      iterateW,
      list,
      unfold,

      -- * Forcing
      nf,
      whnf
    )
    where

import Control.Applicative
import Control.DeepSeq
import Control.Wire.State
import Control.Wire.Wire
import Data.Monoid


-- | Iterate from the given start value using the given step function.
-- Step each given number of time units.  May skip values depending on
-- the granularity of your clock resp. your framerate.

iterateW ::
    (HasTime t s, Monad m)
    => t         -- ^ Positive time interval before next value.
    -> (b -> b)  -- ^ Step size.
    -> b         -- ^ Start value.
    -> Wire s e m a b
iterateW int _ | int <= 0 = error "iterateW: Non-positive time interval"
iterateW int f = unfold int ((\x -> (x, x)) . f)


-- | Produce a staircase of the values in the given list.  For each @(x,
-- t)@ in the list, the value @x@ is produced for the duration @t@.  The
-- duration must be non-negative.
--
-- * Inhibits: after all values have been produced.

list ::
    (HasTime t s, Monad m, Monoid e)
    => [(b, t)]
    -> Wire s e m a b
list [] = empty
list xs' =
    mkPure $ \ds _ ->
        let (mx, xs) = next (dtime ds) xs' in
        (mx, list xs)

    where
    next _ [] = (Left mempty, [])
    next ds ((x, t) : xs)
        | t < 0     = error "listW: Negative time interval"
        | ds < t    = (Right x, (x, t - ds) : xs)
        | otherwise = next (ds - t) xs


-- | Force the signal into normal form.
--
-- * Depends: now.

nf :: (Monad m, Monoid s, NFData a) => Wire s e m a a
nf = mkPure_ $ \_ x -> x `deepseq` Right x


-- | Current local time starting from zero.

time :: (HasTime t s, Monad m) => Wire s e m a t
time = timeFrom 0


-- | Same as 'realToFrac' applied to current local time.

timeF :: (Fractional b, HasTime t s, Monad m) => Wire s e m a b
timeF = fmap realToFrac time


-- | Current local time starting from the given value.

timeFrom :: (HasTime t s, Monad m) => t -> Wire s e m a t
timeFrom t' =
    mkPure $ \ds _ ->
        let t = t' + dtime ds
        in t `seq` (Right t, timeFrom t)


-- | Staircase with the given step duration of the given unfold.

unfold ::
    (HasTime t s, Monad m)
    => t              -- ^ Step duration.
    -> (g -> (b, g))  -- ^ Unfold.
    -> g              -- ^ Start value.
    -> Wire s e m a b
unfold int _ | int <= 0 = error "unfold: Non-positive step duration"
unfold int gen = uncurry (loop int) . gen
    where
    loop t' x' g' =
        mkPure $ \ds _ ->
            let (t, x, g) = next (dtime ds) t' x' g'
            in (Right x, loop t x g)

    next dt t x' g'
        | dt < t    = (t - dt, x', g')
        | otherwise =
            let (x, g) = gen g' in
            next (dt - t) int x g


-- | Force the signal into weak head normal form.  Wires are expected to
-- ensure WHNF signals on their own, so you shouldn't normally need
-- this.
--
-- * Depends: now.

whnf :: (Monad m, Monoid s) => Wire s e m a a
whnf = mkPure_ $ \_ x -> x `seq` Right x
