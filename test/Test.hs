-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Arrow
import Control.Wire
import Control.Wire.FRP
import Prelude hiding ((.), id)
import Text.Printf


type MyWire = Wire' IO


wire :: MyWire a (Double, Double, Double)
wire =
    liftA3 (,,) lowPeak highPeak id . framerate

    where
    t :: MyWire a Double
    t = fmap realToFrac time


main :: IO ()
main =
    testWire_ 1000 clockSession_ wire
