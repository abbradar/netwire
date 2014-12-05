-- |
-- Module:     Control.Wire.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Event
    ( -- * Events
      EventLike(toEvent),
      Event,
      UniqueEvent,

      -- * Time-based
      at,
      never,
      now,
      periodic,
      periodicList,

      -- * Signal analysis
      became,
      noLonger,

      -- * Extracting
      onUEventM,
      onU,
      occurredU,
      eventU,

      -- * Modifiers
      (<&),
      (&>),
      (<!>),
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
import Control.Monad
import Control.Monad.Fix
import Control.Wire.Core
import Control.Wire.Session
import Control.Wire.Unsafe.Event
import Data.Fixed
import Data.Monoid


-- | Merge events with the leftmost event taking precedence.  Equivalent
-- to using the monoid interface with 'First'.  Infixl 5.
--
-- * Depends: now on both.
--
-- * Inhibits: when any of the two wires inhibit.

(<&) :: (Monad m, EventLike ev) => Wire s e m a (ev b) -> Wire s e m a (ev b) -> Wire s e m a (ev b)
(<&) = liftA2 mergeL

infixl 5 <&


-- | Merge events with the rightmost event taking precedence.
-- Equivalent to using the monoid interface with 'Last'.  Infixl 5.
--
-- * Depends: now on both.
--
-- * Inhibits: when any of the two wires inhibit.

(&>) :: (Monad m, EventLike ev) => Wire s e m a (ev b) -> Wire s e m a (ev b) -> Wire s e m a (ev b)
(&>) = liftA2 mergeR

infixl 5 &>


-- | Merge events discarding their values. Infixl 5.
--
-- * Depends: now on both.
--
-- * Inhibits: when any of the two wires inhibit.

(<!>) :: (Monad m, EventLike ev) => Wire s e m a (ev e1) -> Wire s e m a (ev e2) -> Wire s e m a (ev ())
(<!>) = liftA2 mergeD

infixl 5 <!>


-- | Left scan for events.  Each time an event occurs, apply the given
-- function.
--
-- * Depends: now.

accumE ::
    (EventLike ev)
    => (b -> a -> b)  -- ^ Fold function
    -> b            -- ^ Initial value.
    -> Wire s e m (ev a) (ev b)
accumE f = loop
    where
    loop x' =
        mkSFN $
            event (fromEvent NoEvent, loop x')
                  (\y -> let x = f x' y in (fromEvent $ Event x, loop x))


-- | Left scan for events with no initial value.  Each time an event
-- occurs, apply the given function.  The first event is produced
-- unchanged.
--
-- * Depends: now.

accum1E ::
    (EventLike ev)
    => (a -> a -> a)  -- ^ Fold function
    -> Wire s e m (ev a) (ev a)
accum1E f = initial
    where
    initial =
        mkSFN $ event (fromEvent NoEvent, initial) (fromEvent . Event &&& accumE f)


-- | At the given point in time.
--
-- * Depends: now when occurring.

at ::
    (HasTime t s)
    => t  -- ^ Time of occurrence.
    -> Wire s e m a (UniqueEvent a)
at t' =
    mkSF $ \ds x ->
        let t = t' - dtime ds
        in if t <= 0
             then (fromEvent $ Event x, never)
             else (fromEvent NoEvent, at t)


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

dropE :: (EventLike ev) => Int -> Wire s e m (ev a) (ev a)
dropE n | n <= 0 = mkId
dropE n =
    fix $ \again ->
    mkSFN $ \mev ->
        (fromEvent NoEvent, if occurred mev then dropE (pred n) else again)


-- | Forget all initial occurrences until the given predicate becomes
-- false.
--
-- * Depends: now.

dropWhileE :: (EventLike ev) => (a -> Bool) -> Wire s e m (ev a) (ev a)
dropWhileE p =
    fix $ \again ->
    mkSFN $ \mev ->
        case toEvent mev of
          Event x | not (p x) -> (mev, mkId)
          _ -> (fromEvent NoEvent, again)


-- | Forget all occurrences for which the given predicate is false.
--
-- * Depends: now.

filterE :: (EventLike ev) => (a -> Bool) -> Wire s e m (ev a) (ev a)
filterE p =
    mkSF_ $ \mev ->
        case toEvent mev of
          Event x | p x -> mev
          _ -> fromEvent NoEvent


-- | On each occurrence, apply the function the event carries.
--
-- * Depends: now.

iterateE :: (EventLike ev) => a -> Wire s e m (ev (a -> a)) (ev a)
iterateE = accumE (\x f -> f x)


-- | Maximum of all events.
--
-- * Depends: now.

maximumE :: (EventLike ev, Ord a) => Wire s e m (ev a) (ev a)
maximumE = accum1E max


-- | Minimum of all events.
--
-- * Depends: now.

minimumE :: (EventLike ev, Ord a) => Wire s e m (ev a) (ev a)
minimumE = accum1E min


-- | Left-biased event merge.

mergeL :: (EventLike ev) => ev a -> ev a -> ev a
mergeL = merge const


-- | Right-biased event merge.

mergeR :: (EventLike ev) => ev a -> ev a -> ev a
mergeR = merge (const id)


-- | Event merge discarding data.

mergeD :: (EventLike ev) => ev a -> ev b -> ev ()
mergeD a b = mergeL (fmap (const ()) a) (fmap (const ()) b)


-- | Never occurs.

never :: (EventLike ev) => Wire s e m a (ev b)
never = mkConst (Right $ fromEvent NoEvent)


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

notYet :: (EventLike ev) => Wire s e m (ev a) (ev a)
notYet =
    mkSFN $ event (fromEvent NoEvent, notYet) (const (fromEvent NoEvent, mkId))


-- | Occurs once immediately.
--
-- * Depends: now when occurring.

now :: Wire s e m a (UniqueEvent a)
now = mkSFN $ \x -> (fromEvent $ Event x, never)


-- | Forget all occurrences except the first.
--
-- * Depends: now when occurring.

once :: (EventLike ev) => Wire s e m (ev a) (UniqueEvent a)
once =
    mkSFN $ \mev ->
        (fromEvent $ toEvent mev, if occurred mev then never else once)


-- | Periodic occurrence with the given time period.  First occurrence
-- is now.
--
-- * Depends: now when occurring.

periodic :: (HasTime t s) => t -> Wire s e m a (UniqueEvent a)
periodic int | int <= 0 = error "periodic: Non-positive interval"
periodic int = mkSFN $ \x -> (fromEvent $ Event x, loop int)
    where
    loop 0 = loop int
    loop t' =
        mkSF $ \ds x ->
            let t = t' - dtime ds
            in if t <= 0
                 then (fromEvent $ Event x, loop (mod' t int))
                 else (fromEvent NoEvent, loop t)


-- | Periodic occurrence with the given time period.  First occurrence
-- is now.  The event values are picked one by one from the given list.
-- When the list is exhausted, the event does not occur again.

periodicList :: (HasTime t s) => t -> [b] -> Wire s e m a (UniqueEvent b)
periodicList int _ | int <= 0 = error "periodic: Non-positive interval"
periodicList _ [] = never
periodicList int (x:xs) = mkSFN $ \_ -> (fromEvent $ Event x, loop int xs)
    where
    loop _ [] = never
    loop 0 xs = loop int xs
    loop t' xs0@(x:xs) =
        mkSF $ \ds _ ->
            let t = t' - dtime ds
            in if t <= 0
                 then (fromEvent $ Event x, loop (mod' t int) xs)
                 else (fromEvent NoEvent, loop t xs0)


-- | Product of all events.
--
-- * Depends: now.

productE :: (EventLike ev, Num a) => Wire s e m (ev a) (ev a)
productE = accumE (*) 1


-- | Sum of all events.
--
-- * Depends: now.

sumE :: (EventLike ev, Num a) => Wire s e m (ev a) (ev a)
sumE = accumE (+) 0


-- | Forget all but the first given number of occurrences.
--
-- * Depends: now.

takeE :: (EventLike ev) => Int -> Wire s e m (ev a) (ev a)
takeE n | n <= 0 = never
takeE n =
    fix $ \again ->
    mkSFN $ \mev ->
        (mev, if occurred mev then takeE (pred n) else again)


-- | Forget all but the initial occurrences for which the given
-- predicate is true.
--
-- * Depends: now.

takeWhileE :: (EventLike ev) => (a -> Bool) -> Wire s e m (ev a) (ev a)
takeWhileE p =
    fix $ \again ->
    mkSFN $ \mev ->
        case toEvent mev of
          Event x | not (p x) -> (fromEvent NoEvent, never)
          _ -> (mev, again)


-- | Each time the given unique event occurs, perform the given action
-- with the value the event carries.  The resulting event carries the
-- result of the action.
--
-- * Depends: now.

onUEventM :: (Monad m) => (a -> m b) -> Wire s e m (UniqueEvent a) (UniqueEvent b)
onUEventM = onEventM


-- | Emit an 'UniqueEvent's value when it arrives.
--
-- * Depends: now.
--
-- * Inhibits: while no event happens.

onU :: (Monoid e) => Wire s e m (UniqueEvent a) a
onU = onE


-- | Did the given unique event occur?

occurredU :: UniqueEvent a -> Bool
occurredU = occurred


-- | Fold the given unique event.

eventU :: b -> (a -> b) -> UniqueEvent a -> b
eventU = event
