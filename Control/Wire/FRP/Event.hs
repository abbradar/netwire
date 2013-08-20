-- |
-- Module:     Control.Wire.FRP.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Event
    ( -- * Events
      Event,

      -- * Constructing events
      at,
      never,
      now,
      periodically,

      -- * Manipulating events
      once,
      takeE
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Wire.FRP.Event.Unsafe
import Control.Wire.State
import Control.Wire.Wire
import Data.List
import Data.Semigroup


-- | Occurs at the given times.
--
-- * Depends: now when occurring.

at ::
    (HasTime t s, Monad m, Semigroup a)
    => [t] -> Wire s e m a (Event a)
at = loop 0 . sort
    where
    loop t' ets' =
        mkPure $ \ds x ->
            let t = t' + dtime ds
                (ev, ets) = first (foldl' (<>) mempty .
                                   ((Event $! x) <$))
                                  (span (<= t) ets')
            in (Right ev, loop t ets)


-- | Fold the event.

event :: b -> (a -> b) -> Event a -> b
event _ e (Event x) = e x
event n _ NoEvent   = n


-- | Never occurs.

never :: Event a
never = NoEvent


-- | Occurs now, i.e. at local time 0.
--
-- * Depends: now when occurring.

now :: (Monad m, Monoid s) => Wire s e m a (Event a)
now =
    mkPure $ \_ x -> (Right (Event x), pure never)


-- | Only the first occurrence.
--
-- * Depends: now.

once :: (Monad m, Monoid s) => Wire s e m (Event a) (Event a)
once =
    mkPure $ \_ ev ->
        (Right ev, event once (const (pure never)) ev)


-- | Occurs periodically, including now.
--
-- * Depends: now when occurring.

periodically ::
    (HasTime t s, Monad m, Semigroup a)
    => t
    -> Wire s e m a (Event a)
periodically t = at (iterate (+ t) 0)


-- | Only the first given number of occurrences.
--
-- * Depends: now.

takeE :: (Monad m, Monoid s) => Int -> Wire s e m (Event a) (Event a)
takeE n | n <= 0 = pure never
takeE n =
    mkPure $ \_ ev ->
        (Right ev, event (takeE n) (const (takeE $ pred n)) ev)
