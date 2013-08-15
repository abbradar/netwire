-- |
-- Module:     Control.Wire.FRP.Occasion
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Occasion
    ( -- * Time
      after,
      for
    )
    where

import Control.Applicative
import Control.Wire.FRP.Types
import Control.Wire.State
import Control.Wire.Wire
import Data.Monoid


-- | After the given interval has passed.
--
-- * Depends: now.
--
-- * Inhibits: until the given interval has passed.

after ::
    (HasTime t ds, Monad m, Monoid e)
    => t
    -> Occasion ds e m a
after t' =
    mkPure $ \ds x ->
        let t = t' - dtime ds in
        if t <= 0
          then (Right x, identity)
          else (Left mempty, after t)


-- | For the given interval.
--
-- * Depends: now.
--
-- * Inhibits: after the given interval has passed.

for ::
    (HasTime t ds, Monad m, Monoid e)
    => t
    -> Occasion ds e m a
for t' =
    mkPure $ \ds x ->
        let t = t' - dtime ds in
        if t <= 0
          then (Left mempty, empty)
          else (Right x, for t)
