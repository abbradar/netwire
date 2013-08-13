-- |
-- Module:     Control.Wire.FRP.Types
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Types
    ( -- * Helper types
      Event,
      Wire'
    )
    where

import Control.Wire.State
import Control.Wire.Wire
import Data.Time.Clock


-- | Signals that represent discrete events.

type Event = Maybe


-- | Simple wires.

type Wire' = Wire (Timed NominalDiffTime ()) ()
