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


wire :: WireP' a Double
wire =
    stdNoise 0.01 15

    where
    t :: WireP' a Double
    t = fmap realToFrac time


main :: IO ()
main =
    testWire_ 0 clockSession_ wire
