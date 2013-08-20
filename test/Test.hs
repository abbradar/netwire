-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Wire
import Control.Wire.FRP
import Data.Either
import Data.Traversable (sequenceA)
import Prelude hiding ((.), id)
import Text.Printf


wire :: WireP' a String
wire =
    unwords <$> sequenceA [fmap show time, "def"]


main :: IO ()
main =
    testWire_ 0 clockSession_ wire
