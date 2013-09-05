-- |
-- Module:     Control.Wire.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Event
    ( -- * Events
      Event(..),
      event,
      occurred,

      -- * Time-based
      at,
      never,
      now,
      periodic,

      -- * Signal analysis
      became,
      noLonger,

      -- * Modifiers
      (<&),
      (&>),
      dropE,
      dropWhileE,
      filterE,
      merge,
      mergeL,
      mergeR,
      notYet,
      once,
      takeE,
      takeWhileE,

      -- * Scans
      accumE,
      accum1E,
      iterateE,
      -- ** Special scans
      maximumE,
      minimumE,
      productE,
      sumE
    )
    where

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad.Fix
import Control.Wire.Core
import Control.Wire.Session
import Data.Fixed
import Data.Semigroup
import Data.Typeable


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


-- | Merge events with the leftmost event taking precedence.  Equivalent
-- to using the monoid interface with 'First'.  Infixl 5.
--
-- * Depends: now on both.
--
-- * Inhibits: when any of the two wires inhibit.

(<&) :: (Monad m) => Wire s e m a (Event b) -> Wire s e m a (Event b) -> Wire s e m a (Event b)
(<&) = liftA2 (merge const)

infixl 5 <&


-- | Merge events with the rightmost event taking precedence.
-- Equivalent to using the monoid interface with 'Last'.  Infixl 5.
--
-- * Depends: now on both.
--
-- * Inhibits: when any of the two wires inhibit.

(&>) :: (Monad m) => Wire s e m a (Event b) -> Wire s e m a (Event b) -> Wire s e m a (Event b)
(&>) = liftA2 (merge (const id))

infixl 5 &>


-- | Left scan for events.  Each time an event occurs, apply the given
-- function.
--
-- * Depends: now.

accumE ::
    (b -> a -> b)  -- ^ Fold function
    -> b           -- ^ Initial value.
    -> Wire s e m (Event a) (Event b)
accumE f = loop
    where
    loop x' =
        mkSFN $
            event (NoEvent, loop x')
                  (\y -> let x = f x' y in (Event x, loop x))


-- | Left scan for events with no initial value.  Each time an event
-- occurs, apply the given function.  The first event is produced
-- unchanged.
--
-- * Depends: now.

accum1E ::
    (a -> a -> a)  -- ^ Fold function
    -> Wire s e m (Event a) (Event a)
accum1E f = initial
    where
    initial =
        mkSFN $ event (NoEvent, initial) (Event &&& accumE f)


-- | At the given point in time.
--
-- * Depends: now when occurring.

at ::
    (HasTime t s)
    => t  -- ^ Time of occurrence.
    -> Wire s e m a (Event a)
at t' =
    mkSF $ \ds x ->
        let t = t' - dtime ds
        in if t <= 0
             then (Event x, never)
             else (NoEvent, at t)


-- | Occurs each time the predicate becomes true for the input signal,
-- for example each time a given threshold is reached.
--
-- * Depends: now.

became :: (a -> Bool) -> Wire s e m a (Event a)
became p = off
    where
    off = mkSFN $ \x -> if p x then (Event x, on) else (NoEvent, off)
    on = mkSFN $ \x -> (NoEvent, if p x then on else off)


-- | Forget the first given number of occurrences.
--
-- * Depends: now.

dropE :: Int -> Wire s e m (Event a) (Event a)
dropE n | n <= 0 = mkId
dropE n =
    fix $ \again ->
    mkSFN $ \mev ->
        (NoEvent, if occurred mev then dropE (pred n) else again)


-- | Forget all initial occurrences until the given predicate becomes
-- false.
--
-- * Depends: now.

dropWhileE :: (a -> Bool) -> Wire s e m (Event a) (Event a)
dropWhileE p =
    fix $ \again ->
    mkSFN $ \mev ->
        case mev of
          Event x | not (p x) -> (mev, mkId)
          _ -> (NoEvent, again)


-- | Fold the given event.

event :: b -> (a -> b) -> Event a -> b
event _ j (Event x) = j x
event n _ NoEvent   = n


-- | Forget all occurrences for which the given predicate is false.
--
-- * Depends: now.

filterE :: (a -> Bool) -> Wire s e m (Event a) (Event a)
filterE p =
    mkSF_ $ \mev ->
        case mev of
          Event x | p x -> mev
          _ -> NoEvent


-- | On each occurrence, apply the function the event carries.
--
-- * Depends: now.

iterateE :: a -> Wire s e m (Event (a -> a)) (Event a)
iterateE = accumE (\x f -> f x)


-- | Maximum of all events.
--
-- * Depends: now.

maximumE :: (Ord a) => Wire s e m (Event a) (Event a)
maximumE = accum1E max


-- | Minimum of all events.
--
-- * Depends: now.

minimumE :: (Ord a) => Wire s e m (Event a) (Event a)
minimumE = accum1E min


-- | Merge two events using the given function when both occur at the
-- same time.

merge :: (a -> a -> a) -> Event a -> Event a -> Event a
merge _ NoEvent NoEvent     = NoEvent
merge _ (Event x) NoEvent   = Event x
merge _ NoEvent (Event y)   = Event y
merge f (Event x) (Event y) = Event (f x y)


-- | Left-biased event merge.

mergeL :: Event a -> Event a -> Event a
mergeL = merge const


-- | Right-biased event merge.

mergeR :: Event a -> Event a -> Event a
mergeR = merge (const id)


-- | Never occurs.

never :: Wire s e m a (Event b)
never = mkConst (Right NoEvent)


-- | Occurs each time the predicate becomes false for the input signal,
-- for example each time a given threshold is no longer exceeded.
--
-- * Depends: now.

noLonger :: (a -> Bool) -> Wire s e m a (Event a)
noLonger p = off
    where
    off = mkSFN $ \x -> if p x then (NoEvent, off) else (Event x, on)
    on = mkSFN $ \x -> (NoEvent, if p x then off else on)


-- | Forget the first occurrence.
--
-- * Depends: now.

notYet :: Wire s e m (Event a) (Event a)
notYet =
    mkSFN $ event (NoEvent, notYet) (const (NoEvent, mkId))


-- | Occurs once immediately.
--
-- * Depends: now when occurring.

now :: Wire s e m a (Event a)
now = mkSFN $ \x -> (Event x, never)


-- | Did the given event occur?

occurred :: Event a -> Bool
occurred = event False (const True)


-- | Forget all occurrences except the first.
--
-- * Depends: now when occurring.

once :: Wire s e m (Event a) (Event a)
once =
    mkSFN $ \mev ->
        (mev, if occurred mev then never else once)


-- | Periodic occurrence with the given time distance @d@.  First
-- occurrence is at @d@.
--
-- * Depends: now when occurring.

periodic ::
    (HasTime t s)
    => t
    -> Wire s e m a (Event a)
periodic int | int <= 0 = error "periodic: Non-positive interval"
periodic int = loop int
    where
    loop 0 = loop int
    loop t' =
        mkSF $ \ds x ->
            let t = t' - dtime ds
            in if t <= 0
                 then (Event x, loop (mod' t int))
                 else (NoEvent, loop t)


-- | Product of all events.
--
-- * Depends: now.

productE :: (Num a) => Wire s e m (Event a) (Event a)
productE = accumE (*) 1


-- | Sum of all events.
--
-- * Depends: now.

sumE :: (Num a) => Wire s e m (Event a) (Event a)
sumE = accumE (+) 0


-- | Forget all but the first given number of occurrences.
--
-- * Depends: now.

takeE :: Int -> Wire s e m (Event a) (Event a)
takeE n | n <= 0 = never
takeE n =
    fix $ \again ->
    mkSFN $ \mev ->
        (mev, if occurred mev then takeE (pred n) else again)


-- | Forget all but the initial occurrences for which the given
-- predicate is true.
--
-- * Depends: now.

takeWhileE :: (a -> Bool) -> Wire s e m (Event a) (Event a)
takeWhileE p =
    fix $ \again ->
    mkSFN $ \mev ->
        case mev of
          Event x | not (p x) -> (NoEvent, never)
          _ -> (mev, again)
