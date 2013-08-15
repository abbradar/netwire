-- |
-- Module:     Control.Wire.FRP.Switch
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Switch
    ( -- * Simple switches
      (-->)
    )
    where

import Control.Wire.Wire


(-->) ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m a b
    -> Wire s e m a b
Wire f --> w2 =
    Wire $ \ds mx' -> do
        (mx, w1) <- f ds mx'
        case mx of
          Left _  -> stepWire w2 ds mx'
          Right _ -> return (mx, w1 --> w2)

infixr 1 -->
