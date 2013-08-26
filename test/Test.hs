-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Wire
import Control.Wire.FRP
import Prelude hiding ((.), id)


wire :: (Enum t, Fractional t, HasTime t s, Monad m, Show t) => Wire s () m a String
wire = hold "x" . nf . atList [1..] . nf . fmap show time


main :: IO ()
main = testWire_ 0 clockSession_ wire
