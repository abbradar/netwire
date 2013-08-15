-- |
-- Module:     Control.Wire.FRP.Core
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Core
    ( -- * Time
      time,
      timeFrom,

      -- * Unfold
      iterateW
    )
    where

import Control.Wire.State
import Control.Wire.Wire


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
iterateW int f = loop 0
    where
    loop t' x' =
        mkPure $ \ds _ ->
            let (t, x) = step (t' + dtime ds) x' in
            (Right x, loop t x)

    step t x | t >= int   = step (t - int) $! f x
             | otherwise = x `seq` (t, x)


-- | Current local time starting from zero.

time :: (HasTime t s, Monad m) => Wire s e m a t
time = timeFrom 0


-- | Current local time starting from the given value.

timeFrom :: (HasTime t s, Monad m) => t -> Wire s e m a t
timeFrom t' =
    mkPure $ \ds _ ->
        let t = t' + dtime ds
        in t `seq` (Right t, timeFrom t)
