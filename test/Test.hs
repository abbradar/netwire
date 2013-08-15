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


wire :: WireP' a String
wire =
    "hey!" . for 2 -->
    "yo!" . for 2 -->
    "groovy!" . for 2 -->
    fmap show (timeFrom 6)

    where
    t :: WireP' a Double
    t = fmap realToFrac time


main :: IO ()
main =
    testWire_ 1000 clockSession_ wire
