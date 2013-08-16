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

      -- * Events
      Event(..),
      event
    )
    where

import Control.Applicative
import Control.Wire.State
import Control.Wire.Wire
import Data.Data
import Data.Foldable (Foldable)
import Data.Monoid
import Data.String
import Data.Time.Clock
import Data.Traversable (Traversable)


-- | Signals that represent discrete events.

data Event a = Event a | NoEvent
    deriving (Data, Eq, Foldable, Functor,
              Ord, Read, Show, Traversable, Typeable)

instance Alternative Event where
    empty = NoEvent

    Event x <|> _ = Event x
    _ <|> Event y = Event y
    _ <|> _       = NoEvent

instance Applicative Event where
    pure = Event

    Event f <*> Event x = Event (f x)
    _       <*> _       = NoEvent

instance (Floating a) => Floating (Event a) where
    (**) = liftA2 (**)
    acos = fmap acos
    acosh = fmap acosh
    asin = fmap asin
    asinh = fmap asinh
    atan = fmap atan
    atanh = fmap atanh
    cos = fmap cos
    cosh = fmap cosh
    exp = fmap exp
    log = fmap log
    logBase = liftA2 logBase
    pi = pure pi
    sin = fmap sin
    sinh = fmap sinh
    sqrt = fmap sqrt
    tan = fmap tan
    tanh = fmap tanh

instance (Fractional a) => Fractional (Event a) where
    (/)   = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational

instance (IsString a) => IsString (Event a) where
    fromString = pure . fromString

instance (Monoid a) => Monoid (Event a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance (Num a) => Num (Event a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs    = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger


-- | Occasions are identity-like wires that may inhibit.

type Occasion s e m a = Wire s e m a a


-- | Simple wires.

type Wire' = Wire (Timed NominalDiffTime ()) ()


-- | Simple pure wires.

type WireP' a b = WireP (Timed NominalDiffTime ()) () a b


-- | Fold an event.

event :: b -> (a -> b) -> Event a -> b
event _ e (Event x) = e x
event n _ NoEvent   = n
