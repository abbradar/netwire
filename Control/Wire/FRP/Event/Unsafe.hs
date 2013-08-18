-- |
-- Module:     Control.Wire.FRP.Event.Unsafe
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Event.Unsafe
    ( -- * Events
      Event(..)
    )
    where

import Data.Monoid
import Data.Typeable


-- | Denotes a stream of values, each together with time of occurrence.

data Event a = Event a | NoEvent  deriving (Typeable)

instance (Monoid a) => Monoid (Event a) where
    mempty = NoEvent

    mappend (Event x) (Event y) = Event $! x <> y
    mappend (Event x) _         = Event x
    mappend _ (Event y)         = Event y
    mappend NoEvent NoEvent     = NoEvent
