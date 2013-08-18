-- |
-- Module:     Control.Wire.FRP.Switch
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Switch
    ( -- * Occasion-based switches
      (-->),

      -- * Event-based switches
      switch
    )
    where

import Control.Wire.FRP.Event.Unsafe
import Control.Wire.Wire
import Data.Monoid


-- | Act like the first wire as long as it produces, then switch to the
-- second one.  Right-associative, precedence 1.
--
-- * Switch: now, keep state.

(-->) ::
    (Monad m, Monoid e, Monoid s)
    => Wire s e m a b
    -> Wire s e m a b
    -> Wire s e m a b
Wire f --> w2' =
    mkWire $ \ds x' -> do
        (mx, w1) <- f ds (Right x')
        case mx of
          Left _  -> stepWire w2' ds (Right x')
          Right _ -> do
              (_, w2) <- stepWire w2' ds (Left mempty)
              return (mx, w1 --> w2)

infixr 1 -->


-- | Start with the given wire.  As soon as its event occurs, switch to
-- the wire in the event's value.
--
-- * Switch: now, restart state.

switch ::
    (Monad m, Monoid s)
    => Wire s e m a (b, Event (Wire s e m a b))
    -> Wire s e m a b
switch w' =
    mkWire $ \ds x' -> do
        (mx, w) <- stepWire w' ds (Right x')
        case mx of
          Left _ -> return (fmap fst mx, switch w)
          Right (x, NoEvent) -> return (Right x, switch w)
          Right (_, Event w1) -> stepWire w1 mempty (Right x')

infixr 1 `switch`
