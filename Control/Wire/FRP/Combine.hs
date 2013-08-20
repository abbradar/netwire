-- |
-- Module:     Control.Wire.FRP.Combine
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Combine
    ( -- * Broadcast
      broadcast
    )
    where

import qualified Data.Traversable as T
import Control.Wire.Wire
import Data.Traversable (Traversable)


-- | Broadcast to a collection of wires.
--
-- * Depends: like the strictest component wire.

broadcast ::
    (Monad m, Traversable f)
    => (e -> c)  -- ^ Inhibited signals.
    -> (b -> c)  -- ^ Produced signals.
    -> f (Wire s e m a b)  -- ^ Collection of wires.
    -> Wire s e m a (f c)
broadcast f g ws' =
    Wire $ \ds mx' -> do
        mxs <- T.mapM (\w' -> stepWire w' ds mx') ws'
        return (Right $ fmap (either f g . fst) mxs, broadcast f g (fmap snd mxs))
