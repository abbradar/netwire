-- |
-- Module:     Control.Wire.Semantics
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- This module is intended to define and experiment with the FRP
-- semantics of Netwire.  You should not need it, unless you want to
-- become a Netwire hacker.

module Control.Wire.Semantics
    ( -- * Event semantics
      -- $events
      Event(..),
      -- ** Constructing
      at,
      never,
      now,
      periodically,
      -- ** Combining
      -- ** Manipulating
      delayE,
      dropE,
      notYet,
      once,
      takeE
    )
    where

import Control.Arrow
import Data.Monoid


-- Event semantics
-- ===============
--
-- $events Events are viewed as a list of values together with their
-- occurrence times.  Time is relative to /now/, that is t = 0
-- corresponds to now and t > 0 corresponds to the future.  We consider
-- past events to be forgotten, so time must always be non-negative.


-- | Events with time relative to /now/.  We assume the list to be
-- sorted and the times to be unique.  For most event operations the
-- value type is required to be a monoid to handle simultaneity.

newtype Event t a = Event [(t, a)]
    deriving (Functor)


-- | Occurs the given times into the future.

at :: (Monoid a) => [t] -> Event t a
at = Event . map (, mempty)


-- | Delay the given event.  The delay argument must be non-negative.

delayE :: (Num t) => t -> Event t a -> Event t a
delayE dt (Event evs) = Event (map (first (+ dt)) evs)


-- | Drop the given number of first events.

dropE :: Int -> Event t a -> Event t a
dropE n (Event evs) = Event (drop n evs)


-- | Never occurs.

never :: Event t a
never = Event []


-- | Forget occurrence at now.

notYet :: (Eq t, Num t) => Event t a -> Event t a
notYet (Event evs) = Event (filter ((== 0) . fst) evs)


-- | Occurs right now.

now :: (Monoid a, Num t) => Event t a
now = at [0]


-- | Forget every occurrence after the first.

once :: Event t a -> Event t a
once (Event evs) = Event (take 1 evs)


-- | Occurs periodically, including now.

periodically :: (Monoid a, Num t) => t -> Event t a
periodically dt = at . iterate (+ dt) $ 0


-- | Take the given number of first events.

takeE :: Int -> Event t a -> Event t a
takeE n (Event evs) = Event (take n evs)
