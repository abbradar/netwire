-- |
-- Module:     Control.Wire.Unsafe.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Unsafe.Event
    ( -- * Events
      EventLike(..),
      Event(..),
      UniqueEvent(..),
      coerceEvent,
      -- * Helper functions
      event,
      merge,
      occurred,
      onEventM,
      onE
    )
    where

import Control.DeepSeq
import Control.Monad
import Control.Wire.Core
import Data.Semigroup
import Data.Typeable


-- | Class of various events that can be converted into the abstract
--   'Event'.

class Functor e => EventLike e where
  toEvent :: e a -> Event a
  fromEvent :: Event a -> e a


-- | Denotes a stream of values, each together with time of occurrence.
-- Since 'Event' is commonly used for functional reactive programming it
-- does not define most of the usual instances to protect continuous
-- time and discrete event occurrence semantics.

data Event a = Event a | NoEvent  deriving (Typeable)

instance Functor Event where
    fmap f = event NoEvent (Event . f)

instance (Semigroup a) => Monoid (Event a) where
    mempty = NoEvent
    mappend = (<>)

instance (NFData a) => NFData (Event a) where
    rnf (Event x) = rnf x
    rnf NoEvent   = ()

instance (Semigroup a) => Semigroup (Event a) where
    (<>) = merge (<>)

instance EventLike Event where
  toEvent = id
  fromEvent = id


-- | An 'Event' which has a proof of uniqueness, that is, each time a
-- wire observes some as its input, it can assume that it's different
-- from before.
--
-- 'UniqueEvent' must *not* be saved in state end emitted later -- this
-- would break its main assumption. Sadly, we can't guarantee this with
-- types yet.

newtype UniqueEvent a = UniqueEvent (Event a)
                      deriving (Typeable,
                               Functor,
                               Monoid,
                               NFData,
                               Semigroup,
                               EventLike)


-- | Coerce between different event types.

coerceEvent :: (EventLike e1, EventLike e2) => e1 a -> e2 a
coerceEvent = fromEvent . toEvent


-- | Fold the given event.

event :: (EventLike e) => b -> (a -> b) -> e a -> b
event n j = event' . toEvent
  where event' (Event x) = j x
        event' NoEvent = n


-- | Merge two events using the given function when both occur at the
-- same time.

merge :: (EventLike e) => (a -> a -> a) -> e a -> e a -> e a
merge f e1 e2 = fromEvent $ case (toEvent e1, toEvent e2) of
                             (NoEvent, NoEvent) -> NoEvent
                             (Event x, NoEvent) -> Event x
                             (NoEvent, Event y) -> Event y
                             (Event x, Event y) -> Event (f x y)


-- | Did the given event occur?

occurred :: (EventLike e) => e a -> Bool
occurred = event False (const True)


-- | Each time the given event occurs, perform the given action with the
-- value the event carries.  The resulting event carries the result of
-- the action.
--
-- * Depends: now.

onEventM :: (EventLike ev, Monad m) => (a -> m b) -> Wire s e m (ev a) (ev b)
onEventM c = mkGen_ $ liftM Right . event (return $ fromEvent NoEvent) (liftM (fromEvent . Event) . c)


-- | Emit an event's value when it arrives.
--
-- * Depends: now.
--
-- * Inhibits: while no event happens.

onE :: (EventLike ev, Monoid e) => Wire s e m (ev a) a
onE = mkPure_ $ event (Left mempty) Right
