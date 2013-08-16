-- |
-- Module:     Control.Wire.FRP.Event
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.FRP.Event
    ( -- * Events
      Event
    )
    where

import Data.Monoid


-- | Denotes a stream of values, each together with time of occurrence.

data Event a = Event a | NoEvent
    deriving (Functor, Show)

instance (Monoid a) => Monoid (Event a) where
    mempty = NoEvent

    mappend (Event x) (Event y) = Event (x <> y)
    mappend (Event x) _ = Event x
    mappend _ (Event y) = Event y
    mappend _ _         = NoEvent
