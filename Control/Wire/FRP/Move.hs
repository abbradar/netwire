-- |
-- Module:     Control.Wire.FRP.Move
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Move
    ( -- * Integrals
      integral,
      integralWith
    )
    where

import Control.Wire.State
import Control.Wire.Wire
import Linear.Vector


-- | Integrate the input signal.
--
-- * Depends: before now.

integral ::
    (Additive f, Fractional a, HasTime t s, Monad m)
    => f a  -- ^ Integration constant (aka start value).
    -> Wire s e m (f a) (f a)
integral x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
        in x' `seq` (Right x', integral (x' ^+^ dt*^dx))


-- | Integrate the left input signal, but apply the given correction
-- function to it.  This can be used to implement collision
-- detection/reaction.
--
-- The right signal of type @w@ is the /world value/.  It is just passed
-- to the correction function for reference and is not used otherwise.
--
-- The correction function must be idempotent with respect to the world
-- value: @f w (f w x) = f w x@.  This is necessary and sufficient to
-- protect time continuity.

integralWith ::
    (Additive f, Fractional a, HasTime t s, Monad m)
    => (w -> f a -> f a)  -- ^ Correction function.
    -> f a                -- ^ Integration constant (aka start value).
    -> Wire s e m (f a, w) (f a)
integralWith correct = loop
    where
    loop x' =
        mkPure $ \ds (dx, w) ->
            let dt = realToFrac (dtime ds)
                x  = correct w (x' ^+^ dt*^dx)
            in x' `seq` (Right x', loop x)
