-- |
-- Module:     Control.Wire.FRP.Occasion
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Occasion
    ( -- * Time
      after,
      for,

      -- * Predicate-based
      asSoonAs,
      unless,
      until,
      when,
      while
    )
    where

import Control.Applicative
import Control.Category
import Control.Wire.FRP.Types
import Control.Wire.State
import Control.Wire.Wire
import Data.Monoid
import Prelude hiding ((.), id, until)


-- | After the given interval has passed.
--
-- * Depends: now.
--
-- * Inhibits: until the given interval has passed.

after ::
    (HasTime t s, Monad m, Monoid e)
    => t
    -> Occasion s e m a
after t' =
    mkPure $ \ds x ->
        let t = t' - dtime ds in
        if t <= 0
          then (Right x, id)
          else (Left mempty, after t)


-- | Produce forever as soon as the given predicate holds.
--
-- * Depends: now.
--
-- * Inhibits: until the given predicate is true.

asSoonAs ::
    (Monad m, Monoid e, Monoid s)
    => (a -> Bool)
    -> Occasion s e m a
asSoonAs p = loop
    where
    loop =
        mkPure $ \_ x ->
            if p x
              then (Right x, id)
              else (Left mempty, loop)


-- | For the given interval.
--
-- * Depends: now.
--
-- * Inhibits: after the given interval has passed.

for ::
    (HasTime t s, Monad m, Monoid e)
    => t
    -> Occasion s e m a
for t' =
    mkPure $ \ds x ->
        let t = t' - dtime ds in
        if t <= 0
          then (Left mempty, empty)
          else (Right x, for t)


-- | When the input does not satisfy the given predicate.
--
-- * Depends: now.
--
-- * Inhibits: when the predicate succeeds.

unless ::
    (Monad m, Monoid e, Monoid s)
    => (a -> Bool)  -- ^ Predicate.
    -> Occasion s e m a
unless p =
    mkPure_ $ \_ x ->
        if p x then Left mempty else Right x


-- | Inhibit forever as soon as the given predicate holds.
--
-- * Depends: now.
--
-- * Inhibits: forever as soon as the given predicate is true.

until ::
    (Monad m, Monoid e, Monoid s)
    => (a -> Bool)
    -> Occasion s e m a
until p = loop
    where
    loop =
        mkPure $ \_ x ->
            if p x
              then (Left mempty, empty)
              else (Right x, loop)


-- | When the input satisfies the given predicate.
--
-- * Depends: now.
--
-- * Inhibits: when the predicate fails.

when ::
    (Monad m, Monoid e, Monoid s)
    => (a -> Bool)  -- ^ Predicate.
    -> Occasion s e m a
when p =
    mkPure_ $ \_ x ->
        if p x then Right x else Left mempty


-- | Inhibit forever as soon as the given predicate fails.
--
-- * Depends: now.
--
-- * Inhibits: forever as soon as the given predicate is false.

while ::
    (Monad m, Monoid e, Monoid s)
    => (a -> Bool)
    -> Occasion s e m a
while p = loop
    where
    loop =
        mkPure $ \_ x ->
            if p x
              then (Right x, loop)
              else (Left mempty, empty)
