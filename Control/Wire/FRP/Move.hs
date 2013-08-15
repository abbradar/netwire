-- |
-- Module:     Control.Wire.FRP.Move
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Move
    ( -- * Integrals
      integral
    )
    where

import Control.Wire.State
import Control.Wire.Wire


-- | Integrate the input signal.
--
-- * Depends: before now.

integral ::
    (Fractional a, HasTime t s, Monad m)
    => a -> Wire s e m a a
integral x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
        in x' `seq` (Right x', integral (x' + dt*dx))
