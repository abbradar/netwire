-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Wire
import Control.Wire.FRP
import Prelude hiding ((.), id)


wire :: (HasTime t s, Monad m) => Wire s () m a t
wire = time


main :: IO ()
main = testWire_ 0 clockSession_ wire
