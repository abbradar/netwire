-- |
-- Module:     Control.Wire.FRP.Types
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Types
    ( -- * Wire synonyms
      Occasion,
      Wire',
      WireP',

      -- * Helper types
      Event
    )
    where

import Control.Wire.State
import Control.Wire.Wire
import Data.Time.Clock


-- | Signals that represent discrete events.

type Event = Maybe


-- | Occasions are identity-like wires that may inhibit.

type Occasion ds e m a = Wire ds e m a a


-- | Simple wires.

type Wire' = Wire (Timed NominalDiffTime ()) ()


-- | Simple pure wires.

type WireP' a b = WireP (Timed NominalDiffTime ()) () a b
