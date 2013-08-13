-- |
-- Module:     Control.Wire.Wire
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Wire
    ( -- * Wires
      Wire(..),

      -- * Helpers
      identity,

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
import Data.Monoid
import Data.String
import Prelude hiding ((.), id)


-- | A wire represents a reactive value that may depend on other
-- reactive values.

newtype Wire ds e m a b =
    Wire {
      stepWire :: ds -> Either e a -> m (Either e b, Wire ds e m a b)
    }

instance (Applicative m, Monoid e) => Alternative (Wire ds e m a) where
    empty = w
        where
        w = Wire (\_ -> pure . (, w) . either Left (const (Left mempty)))

    Wire f <|> Wire g =
        Wire $ \ds mx' ->
            (\(mx1, w1) (mx2, w2) -> (combined mx1 mx2, w1 <|> w2))
            <$> f ds mx'
            <*> g ds mx'

        where
        combined (Right x1) _ = Right x1
        combined _ (Right x2) = Right x2
        combined (Left ex1) (Left ex2) = Left (ex1 <> ex2)

instance (Applicative m) => Applicative (Wire ds e m a) where
    pure x = let w = Wire (\_ mx -> pure (x <$ mx, w)) in w

    Wire ff <*> Wire fx =
        Wire $ \ds mx' ->
            (\(mf, wf) (mx, wx) -> (mf <*> mx, wf <*> wx))
            <$> ff ds mx'
            <*> fx ds mx'

instance (Applicative m, Monad m) => Arrow (Wire ds e m) where
    arr f = let w = Wire (\_ mx -> return (fmap f mx, w)) in w

    first (Wire f) =
        Wire $ \ds mxy' ->
            (\(mx, w) -> (liftA2 (,) mx (fmap snd mxy'), first w))
            <$> f ds (fmap fst mxy')

    second (Wire f) =
        Wire $ \ds mxy' ->
            (\(my, w) -> (liftA2 (,) (fmap fst mxy') my, second w))
            <$> f ds (fmap snd mxy')

    Wire f &&& Wire g =
        Wire $ \ds mx' ->
            (\(mx, w1) (my, w2) -> (liftA2 (,) mx my, w1 &&& w2))
            <$> f ds mx'
            <*> g ds mx'

    Wire f *** Wire g =
        Wire $ \ds mxy' ->
            (\(mx, w1) (my, w2) -> (liftA2 (,) mx my, w1 *** w2))
            <$> f ds (fmap fst mxy')
            <*> g ds (fmap snd mxy')

-- instance (Applicative m, Monad m) => ArrowChoice (Wire ds e m) where
--     left w'@(Wire f) =
--         Wire $ \ds mmx

instance (Applicative m, Monad m, Monoid e) => ArrowPlus (Wire ds e m) where
    (<+>) = (<|>)

instance (Applicative m, Monad m, Monoid e) => ArrowZero (Wire ds e m) where
    zeroArrow = empty

instance (Monad m) => Category (Wire ds e m) where
    id = let w = Wire (\_ mx -> return (mx, w)) in w

    Wire f . Wire g =
        Wire $ \ds mx -> do
            (my, w1) <- g ds mx
            (mz, w2) <- f ds my
            return (mz, w2 . w1)

instance (Applicative m, Floating b) => Floating (Wire ds e m a b) where
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

instance (Applicative m, Fractional b) => Fractional (Wire ds e m a b) where
    (/)   = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational

instance (Functor m) => Functor (Wire ds e m a) where
    fmap f (Wire g) =
        Wire $ \ds -> fmap (fmap f *** fmap f) . g ds

instance (Applicative m, IsString b) => IsString (Wire ds e m a b) where
    fromString = pure . fromString

instance (Applicative m, Monoid b) => Monoid (Wire ds e m a b) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance (Applicative m, Num b) => Num (Wire ds e m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs    = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger


-- | Identity wire.

identity :: (Applicative m) => Wire ds e m a a
identity = let w = Wire (\_ mx -> pure (mx, w)) in w


-- | Construct a pure wire from the given transition function.

mkPure ::
    (Applicative m, Monoid ds)
    => (ds -> a -> (Either e b, Wire ds e m a b))
    -> Wire ds e m a b
mkPure f = mkWire (\ds -> pure . f ds)


-- | Construct a pure stateless wire from the given transition function.

mkPure_ ::
    (Applicative m, Monoid ds)
    => (ds -> a -> Either e b)
    -> Wire ds e m a b
mkPure_ f = let w = mkPure (\ds -> (, w) . f ds) in w


-- | Construct a wire from the given transition function.

mkWire ::
    (Applicative m, Monoid ds)
    => (ds -> a -> m (Either e b, Wire ds e m a b))
    -> Wire ds e m a b
mkWire f = loop mempty
    where
    loop ds' =
        Wire $ \dds mx' ->
            let ds = ds' <> dds in
            ds `seq`
            case mx' of
              Left ex  -> pure (Left ex, loop ds)
              Right x' -> f ds x'


-- | Construct a stateless wire from the given transition function.

mkWire_ ::
    (Applicative m, Monoid ds)
    => (ds -> a -> m (Either e b))
    -> Wire ds e m a b
mkWire_ f = let w = mkWire (\ds -> fmap (, w) . f ds) in w
