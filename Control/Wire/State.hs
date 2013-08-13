-- |
-- Module:     Control.Wire.State
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.State
    ( -- * State delta types
      HasTime(..),
      Session(..),

      -- ** Wires with time
      Timed(..),
      clockSession,
      clockSession_,
      countSession,
      countSession_
    )
    where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable (Foldable)
import Data.Monoid
import Data.Time.Clock
import Data.Traversable (Traversable)


-- | State delta types with time deltas.

class (Monoid ds, Real dt) => HasTime dt ds | ds -> dt where
    -- | Extract the current time delta.
    dtime :: ds -> dt


-- | State delta generators as required for wire sessions, most notably
-- to generate time deltas.  These are mini-wires with the sole purpose
-- of generating these deltas.

newtype Session m ds =
    Session {
      stepSession :: m (ds, Session m ds)
    }
    deriving (Functor)

instance (Applicative m) => Applicative (Session m) where
    pure x = let s = Session (pure (x, s)) in s

    Session ff <*> Session fx =
        Session $ liftA2 (\(f, sf) (x, sx) -> (f x, sf <*> sx)) ff fx


-- | This state delta type denotes time deltas.  This is necessary for
-- most FRP applications.

data Timed dt ds = Timed dt ds
    deriving (Data, Eq, Foldable, Functor,
              Ord, Read, Show, Traversable, Typeable)

instance (Monoid ds, Real dt) => HasTime dt (Timed dt ds) where
    dtime (Timed dt _) = dt

instance (Monoid ds, Num dt) => Monoid (Timed dt ds) where
    mempty = Timed 0 mempty

    mappend (Timed dt1 ds1) (Timed dt2 ds2) =
        Timed (dt1 + dt2) (ds1 <> ds2)


-- | State delta generator for a real time clock.

clockSession :: (MonadIO m) => Session m (ds -> Timed NominalDiffTime ds)
clockSession =
    Session $ do
        t0 <- liftIO getCurrentTime
        return (Timed 0, loop t0)

    where
    loop t' =
        Session $ do
            t <- liftIO getCurrentTime
            let dt = diffUTCTime t t'
            dt `seq` return (Timed dt, loop t)


-- | Non-extending version of 'clockSession'.

clockSession_ :: (Applicative m, MonadIO m) => Session m (Timed NominalDiffTime ())
clockSession_ = clockSession <*> pure ()


-- | State delta generator for a simple counting clock.  Denotes a fixed
-- framerate.  This is likely more useful than 'clockSession' for
-- simulations and real-time games.

countSession ::
    (Applicative m)
    => dt  -- ^ Increment size.
    -> Session m (ds -> Timed dt ds)
countSession dt =
    let loop = Session (pure (Timed dt, loop))
    in loop


-- | Non-extending version of 'countSession'.

countSession_ :: (Applicative m, MonadIO m) => dt -> Session m (Timed dt ())
countSession_ dt = countSession dt <*> pure ()
