-- |
-- Module:     Control.Wire.FRP.Noise
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Noise
    ( -- * Noise generators
      noise,
      noiseR,
      wackelkontakt,

      -- * Convenience
      stdNoise,
      stdNoiseR,
      stdWackelkontakt
    )
    where

import Control.Wire.FRP.Core
import Control.Wire.FRP.Types
import Control.Wire.State
import Control.Wire.Wire
import Data.Monoid
import System.Random


-- | Staircase of noise with the given step duration.

noise ::
    (HasTime t s, Monad m, Random b, RandomGen g)
    => t  -- ^ Step duration.
    -> g  -- ^ Random number generator.
    -> Wire s e m a b
noise int | int <= 0 = error "noise: Non-positive interval"
noise int = unfold int random


-- | Staircase of noise in the given range with the given step duration.

noiseR ::
    (HasTime t s, Monad m, Random b, RandomGen g)
    => t       -- ^ Step duration.
    -> (b, b)  -- ^ Noise range.
    -> g       -- ^ Random number generator.
    -> Wire s e m a b
noiseR int | int <= 0 = error "noiseR: Non-positive interval"
noiseR int = unfold int . randomR


-- | Convenience interface to 'noise' for 'StdGen'.

stdNoise ::
    (HasTime t s, Monad m, Random b)
    => t    -- ^ Step duration.
    -> Int  -- ^ 'StdGen' seed.
    -> Wire s e m a b
stdNoise int = noise int . mkStdGen


-- | Convenience interface to 'noiseR' for 'StdGen'.

stdNoiseR ::
    (HasTime t s, Monad m, Random b)
    => t       -- ^ Step duration.
    -> (b, b)  -- ^ Noise range.
    -> Int     -- ^ 'StdGen' seed.
    -> Wire s e m a b
stdNoiseR int r = noiseR int r . mkStdGen


-- | Convenience interface to 'wackelkontakt' for 'StdGen'.

stdWackelkontakt ::
    (HasTime t s, Monad m, Monoid e)
    => t    -- ^ Step duration.
    -> Double    -- ^ Probability to produce.
    -> Int  -- ^ 'StdGen' seed.
    -> Occasion s e m a
stdWackelkontakt int p = wackelkontakt int p . mkStdGen


-- | Randomly produce or inhibit with the given probability, each time
-- for the given duration.
--
-- The name /Wackelkontakt/ is a Netwire running gag.  It's the German
-- word for slack joint.
--
-- * Depends: now.

wackelkontakt ::
    (HasTime t s, Monad m, Monoid e, RandomGen g)
    => t  -- ^ Step duration.
    -> Double  -- ^ Probability to produce.
    -> g  -- ^ Random number generator.
    -> Occasion s e m a
wackelkontakt int _ | int <= 0 = error "wackelkontakt: Non-positive duration"
wackelkontakt int p = loop . noise int
    where
    loop w' =
        mkWire $ \ds x' -> do
            (~(Right s), w) <- stepWire w' ds (Right ())
            return $
                if s < p
                  then (Right x', loop w)
                  else (Left mempty, loop w)
