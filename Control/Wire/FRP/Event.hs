-- |
-- Module:     Control.Wire.FRP.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Event
    ( -- * Events
      Event,
      -- ** Constructing events
      at,
      never,
      now,
      --once,
      periodically,

      -- * Unsafe interface
      unsafeEvent
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Wire.State
import Control.Wire.Wire
import Data.List
import Data.Monoid
import Data.Typeable


-- | Denotes a stream of values, each together with time of occurrence.

data Event a = Event a | NoEvent  deriving (Show, Typeable)

instance (Monoid a) => Monoid (Event a) where
    mempty = NoEvent

    mappend (Event x) (Event y) = Event $! x <> y
    mappend (Event x) _         = Event x
    mappend _ (Event y)         = Event y
    mappend NoEvent NoEvent     = NoEvent


-- | Occurs at the given times.
--
-- * Depends: now when occurring.

at ::
    (HasTime t s, Monad m, Monoid a)
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


-- | Never occurs.

never :: Event a
never = NoEvent


-- | Occurs now, i.e. at local time 0.
--
-- * Depends: now when occurring.

now :: (Monad m, Monoid s) => Wire s e m a (Event a)
now =
    mkPure $ \_ x -> (Right (Event x), pure NoEvent)


-- | Occurs periodically, including now.
--
-- * Depends: now when occurring.

periodically ::
    (HasTime t s, Monad m, Monoid a)
    => t
    -> Wire s e m a (Event a)
periodically t = at (iterate (+ t) 0)


-- | Construct an event value.  Warning: This function allows you to
-- expose discrete time.  It should only be used by framework developers
-- who develop event wires.

unsafeEvent :: a -> Event a
unsafeEvent = Event
