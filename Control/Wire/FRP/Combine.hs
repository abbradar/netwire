-- |
-- Module:     Control.Wire.FRP.Combine
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Combine
    ( -- * Broadcast
      broadcast,
      --bSwitch
    )
    where

import qualified Data.Traversable as T
import Control.Wire.Wire
import Data.Traversable (Traversable)


-- | Broadcast to the given collection of wires.  If you want to inhibit
-- when one of the component wires inhibits, use 'T.sequenceA'.
--
-- * Depends: like the strictest component wire.

broadcast ::
    (Monad m, Traversable f)
    => f (Wire s e m a b)  -- ^ Collection of wires.
    -> Wire s e m a (f (Either e b))
broadcast ws' =
    Wire $ \ds mx' -> do
        mxs <- T.mapM (\w' -> stepWire w' ds mx') ws'
        return (Right (fmap fst mxs), broadcast (fmap snd mxs))
