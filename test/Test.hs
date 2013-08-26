-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Wire
import Prelude hiding ((.), id)


wire :: (Monad m, Monoid s) => Wire s () m a String
wire = pure "x"


main :: IO ()
main = testWire_ 0 clockSession_ wire
