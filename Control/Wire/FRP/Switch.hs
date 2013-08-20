-- |
-- Module:     Control.Wire.FRP.Switch
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Switch
    ( -- * Event-based switches
      -- ** Intrinsic
      switch,
      dSwitch,
      -- ** Intrinsic continuable
      kSwitch,
      dkSwitch,
      -- ** Extrinsic
      rSwitch,
      drSwitch,
      -- ** Extrinsic continuable
      krSwitch,
      dkrSwitch,

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
-- * Inhibits: like the currently active wire.
--
-- * Switch: now, keep state.

(-->) ::
    (Monad m, Monoid e, Monoid s)
    => Wire s e m a b
    -> Wire s e m a b
    -> Wire s e m a b
Wire f --> w2' =
    Wire $ \ds mx' -> do
        (mx, w1) <- f ds mx'
        case mx of
          Left _ | Right _ <- mx' -> stepWire w2' ds mx'
          _ -> liftM (const mx *** (w1 -->)) (stepWire w2' ds (Left mempty))

infixr 1 -->


-- | Intrinsic continuable switch:  Delayed version of 'kSwitch'.
--
-- * Inhibits: like the first argument wire, like the new wire after
--   switch.  Inhibition of the second argument wire is ignored.
--
-- * Switch: once, after now, restart state.

dkSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, b) (Event (Wire s e m a b -> Wire s e m a b))
    -> Wire s e m a b
dkSwitch (Wire f) (Wire g) =
    Wire $ \ds mx' -> do
        (mx,  w1) <- f ds mx'
        (mev, w2) <- g ds (liftA2 (,) mx' mx)
        let w | Right (Event sw) <- mev = sw w1
              | otherwise = dkSwitch w1 w2
        return (mx, w)


-- | Extrinsic switch:  Delayed version of 'rSwitch'.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, after now, restart state.

drSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b)) b
drSwitch w' =
    Wire $ \ds mx' ->
        let nw w | Right (_, Event w1) <- mx' = w1
                 | otherwise = w
        in liftM (second (drSwitch . nw)) (stepWire w' ds (fmap fst mx'))


-- | Intrinsic switch:  Delayed version of 'switch'.
--
-- * Inhibits: like argument wire until switch, then like the new wire.
--
-- * Switch: once, after now, restart state.

dSwitch ::
    (Monad m)
    => Wire s e m a (b, Event (Wire s e m a b))
    -> Wire s e m a b
dSwitch w' =
    Wire $ \ds mx' -> do
        (mx, w) <- stepWire w' ds mx'
        let nw | Right (_, Event w1) <- mx = w1
               | otherwise = dSwitch w
        return (fmap fst mx, nw)


-- | Extrinsic continuable switch.  Delayed version of 'krSwitch'.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, after now, restart state.

dkrSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b -> Wire s e m a b)) b
dkrSwitch w' =
    Wire $ \ds mx' ->
        let nw w | Right (_, Event f) <- mx' = f w
                 | otherwise = w
        in liftM (second (dkrSwitch . nw)) (stepWire w' ds (fmap fst mx'))


-- | Intrinsic continuable switch:  @kSwitch w1 w2@ starts with @w1@.
-- Its signal is received by @w2@, which may choose to switch to a new
-- wire.  Passes the wire we are switching away from to the new wire,
-- such that it may be reused in it.
--
-- * Inhibits: like the first argument wire, like the new wire after
--   switch.  Inhibition of the second argument wire is ignored.
--
-- * Switch: once, now, restart state.

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
          Right (Event sw) -> stepWire (sw w1) mempty mx'
          _                -> return (mx, kSwitch w1 w2)


-- | Extrinsic continuable switch.  This switch works like 'rSwitch',
-- except that it passes the wire we are switching away from to the new
-- wire.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, now, restart state.

krSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b -> Wire s e m a b)) b
krSwitch w'' =
    Wire $ \ds mx' ->
        let w' | Right (_, Event f) <- mx' = f w''
               | otherwise = w''
        in liftM (second krSwitch) (stepWire w' ds (fmap fst mx'))


-- | Extrinsic switch:  Start with the given wire.  Each time the input
-- event occurs, switch to the wire it carries.
--
-- * Inhibits: like the current wire.
--
-- * Switch: recurrent, now, restart state.

rSwitch ::
    (Monad m)
    => Wire s e m a b
    -> Wire s e m (a, Event (Wire s e m a b)) b
rSwitch w'' =
    Wire $ \ds mx' ->
        let w' | Right (_, Event w1) <- mx' = w1
               | otherwise = w''
        in liftM (second rSwitch) (stepWire w' ds (fmap fst mx'))


-- | Intrinsic switch:  Start with the given wire.  As soon as its event
-- occurs, switch to the wire in the event's value.
--
-- * Inhibits: like argument wire until switch, then like the new wire.
--
-- * Switch: once, now, restart state.

switch ::
    (Monad m, Monoid s)
    => Wire s e m a (b, Event (Wire s e m a b))
    -> Wire s e m a b
switch w' =
    Wire $ \ds mx' -> do
        (mx, w) <- stepWire w' ds mx'
        case mx of
          Right (_, Event w1) -> stepWire w1 mempty mx'
          _                   -> return (fmap fst mx, switch w)
