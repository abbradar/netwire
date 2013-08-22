-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Wire
import Control.Wire.FRP
import Prelude hiding ((.), id)


wire :: (Fractional t, HasTime t s, Monad m) => Wire s () m a t
wire = integral 5 . time - (5 + (time*time)/2)


main :: IO ()
main = testWire_ 0 clockSession_ wire
