-- |
-- Module:     Control.Wire.FRP.Switch
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Switch
    ( -- * Event-based switches
      switch,
      kSwitch,
      rSwitch,

      -- * Occasion-based switches
      (-->)
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Monad
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


-- | Intrinsic recurrent continuation switch:  @kSwitch w1 w2@ starts
-- with @w1@.  Its signal is received by @w2@, which may choose to
-- switch to a new wire.  Passes the wire we are switching away from
-- (the continuation wire) to the new wire, such that it may be reused
-- in it.
--
-- Use this switch, if you need to extend a wire over time.
--
-- * Inhibits: when the current wire inhibits.  Inhibition of the second
--   argument wire is ignored.
--
-- * Switch: now, restart state.

kSwitch ::
    (Monad m, Monoid s)
    => Wire s e m a b
    -> Wire s e m (a, b) (Event (Wire s e m a b -> Wire s e m a b))
    -> Wire s e m a b
kSwitch (Wire f) (Wire g) =
    Wire $ \ds mx' -> do
        (mx,  w1) <- f ds mx'
        (mev, w2) <- g ds (liftA2 (,) mx' mx)
        case mev of
          Right (Event sw) ->
              stepWire (kSwitch (sw w1) w2) mempty mx'
          _ -> return (mx, kSwitch w1 w2)


-- | Extrinsic recurrent switch:  Start with the given wire.  Each time
-- the input event occurs, switch to the wire it carries.
--
-- * Switch: now, restart state.

rSwitch ::
    (Monad m, Monoid s)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b)) b
rSwitch w'' =
    Wire $ \ds mx' ->
        let w' | Right (_, Event w1) <- mx' = w1
               | otherwise = w''
        in liftM (second rSwitch) (stepWire w' ds (fmap fst mx'))


-- | Intrinsic one-time switch:  Start with the given wire.  As soon as
-- its event occurs, switch to the wire in the event's value.
--
-- * Switch: now, restart state.

switch ::
    (Monad m, Monoid s)
    => Wire s e m a (b, Event (Wire s e m a b))
    -> Wire s e m a b
switch w' =
    Wire $ \ds mx' -> do
        (mx, w) <- stepWire w' ds mx'
        case mx of
          Left _              -> return (fmap fst mx, switch w)
          Right (x, NoEvent)  -> return (Right x, switch w)
          Right (_, Event w1) -> stepWire w1 mempty mx'
