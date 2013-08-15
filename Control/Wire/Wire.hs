-- |
-- Module:     Control.Wire.Wire
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Wire
    ( -- * Wires
      Wire(..),
      WireP,

      -- * Helpers
      identity,
      mapWire,

      -- * Low level
      mkPure,
      mkPure_,
      mkWire,
      mkWire_
    )
    where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Monoid
import Data.String
import Prelude hiding ((.), id)


-- | A wire represents a reactive value that may depend on other
-- reactive values.

newtype Wire ds e m a b =
    Wire {
      stepWire :: ds -> Either e a -> m (Either e b, Wire ds e m a b)
    }

instance (Monad m, Monoid e) => Alternative (Wire ds e m a) where
    empty = w
        where
        w = Wire (\_ -> return . (, w) . either Left (const (Left mempty)))

    Wire f <|> Wire g =
        Wire $ \ds mx' ->
            liftM2 (\(mx1, w1) (mx2, w2) -> (combined mx1 mx2, w1 <|> w2))
                   (f ds mx')
                   (g ds mx')

        where
        combined (Right x1) _ = Right x1
        combined _ (Right x2) = Right x2
        combined (Left ex1) (Left ex2) = Left (ex1 <> ex2)

instance (Monad m) => Applicative (Wire ds e m a) where
    pure x = let w = Wire (\_ mx -> return (x <$ mx, w)) in w

    Wire ff <*> Wire fx =
        Wire $ \ds mx' ->
            liftM2 (\(mf, wf) (mx, wx) -> (mf <*> mx, wf <*> wx))
                   (ff ds mx')
                   (fx ds mx')

instance (Monad m) => Arrow (Wire ds e m) where
    arr f = let w = Wire (\_ mx -> return (fmap f mx, w)) in w

    first (Wire f) =
        Wire $ \ds mxy' ->
            liftM (\(mx, w) -> (liftA2 (,) mx (fmap snd mxy'), first w))
                  (f ds (fmap fst mxy'))

    second (Wire f) =
        Wire $ \ds mxy' ->
            liftM (\(my, w) -> (liftA2 (,) (fmap fst mxy') my, second w))
                  (f ds (fmap snd mxy'))

    Wire f &&& Wire g =
        Wire $ \ds mx' ->
            liftM2 (\(mx, w1) (my, w2) -> (liftA2 (,) mx my, w1 &&& w2))
                   (f ds mx')
                   (g ds mx')

    Wire f *** Wire g =
        Wire $ \ds mxy' ->
            liftM2 (\(mx, w1) (my, w2) -> (liftA2 (,) mx my, w1 *** w2))
                   (f ds (fmap fst mxy'))
                   (g ds (fmap snd mxy'))

-- instance (Applicative m, Monad m) => ArrowChoice (Wire ds e m) where
--     left w'@(Wire f) =
--         Wire $ \ds mmx

instance (Monad m, Monoid e) => ArrowPlus (Wire ds e m) where
    (<+>) = (<|>)

instance (Monad m, Monoid e) => ArrowZero (Wire ds e m) where
    zeroArrow = empty

instance (Monad m) => Category (Wire ds e m) where
    id = let w = Wire (\_ mx -> return (mx, w)) in w

    Wire f . Wire g =
        Wire $ \ds mx -> do
            (my, w1) <- g ds mx
            (mz, w2) <- f ds my
            return (mz, w2 . w1)

instance (Monad m, Floating b) => Floating (Wire ds e m a b) where
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

instance (Monad m, Fractional b) => Fractional (Wire ds e m a b) where
    (/)   = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational

instance (Monad m) => Functor (Wire ds e m a) where
    fmap f (Wire g) =
        Wire $ \ds -> liftM (fmap f *** fmap f) . g ds

instance (Monad m, IsString b) => IsString (Wire ds e m a b) where
    fromString = pure . fromString

instance (Monad m, Monoid b) => Monoid (Wire ds e m a b) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Monad m, Num b) => Num (Wire ds e m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs    = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger


-- | Pure wires.

type WireP ds e a b =
    forall m. (Monad m)
    => Wire ds e m a b


-- | Identity wire.

identity :: (Monad m) => Wire ds e m a a
identity = let w = Wire (\_ mx -> return (mx, w)) in w


-- | Apply the given monad morphism to the given wire.

mapWire ::
    (Monad m, Monad m')
    => (forall a. m' a -> m a)
    -> Wire ds e m' a b
    -> Wire ds e m a b
mapWire m (Wire f) =
    Wire $ \ds mx ->
        liftM (second (mapWire m)) (m (f ds mx))


-- | Construct a pure wire from the given transition function.

mkPure ::
    (Monad m, Monoid ds)
    => (ds -> a -> (Either e b, Wire ds e m a b))
    -> Wire ds e m a b
mkPure f = mkWire (\ds -> return . f ds)


-- | Construct a pure stateless wire from the given transition function.

mkPure_ ::
    (Monad m, Monoid ds)
    => (ds -> a -> Either e b)
    -> Wire ds e m a b
mkPure_ f = let w = mkPure (\ds -> (, w) . f ds) in w


-- | Construct a wire from the given transition function.

mkWire ::
    (Monad m, Monoid ds)
    => (ds -> a -> m (Either e b, Wire ds e m a b))
    -> Wire ds e m a b
mkWire f = loop mempty
    where
    loop ds' =
        Wire $ \dds mx' ->
            let ds = ds' <> dds in
            ds `seq`
            case mx' of
              Left ex  -> return (Left ex, loop ds)
              Right x' -> f ds x'


-- | Construct a stateless wire from the given transition function.

mkWire_ ::
    (Monad m, Monoid ds)
    => (ds -> a -> m (Either e b))
    -> Wire ds e m a b
mkWire_ f = let w = mkWire (\ds -> liftM (, w) . f ds) in w
