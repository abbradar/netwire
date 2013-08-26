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

import Control.DeepSeq
import Data.Semigroup
import Data.Typeable


-- | Denotes a stream of values, each together with time of occurrence.

data Event a = Event !a | NoEvent  deriving (Typeable)

instance Functor Event where
    fmap f (Event x) = Event (f x)
    fmap _ NoEvent   = NoEvent

instance (Semigroup a) => Monoid (Event a) where
    mempty = NoEvent
    mappend = (<>)

instance (NFData a) => NFData (Event a) where
    rnf (Event x) = rnf x
    rnf NoEvent   = ()

instance (Semigroup a) => Semigroup (Event a) where
    Event x <> Event y = Event (x <> y)
    Event x <> _       = Event x
    _ <> Event y       = Event y
    NoEvent <> NoEvent = NoEvent
