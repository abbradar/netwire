-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Arrow
import Control.Wire
import Control.Wire.FRP
import Linear
import Prelude hiding ((.), id)
import Text.Printf


wire :: WireP' a (V2 Double)
wire =
    proc _ -> do
        integralWith (\_ -> signorm) (V2 1 0) -< (V2 0 1, ())

    where
    t :: WireP' a Double
    t = fmap realToFrac time


main :: IO ()
main =
    testWire_ 1000 clockSession_ wire
