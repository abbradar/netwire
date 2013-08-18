-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import qualified Control.Wire.Timeline as Tl
import Control.Arrow
import Control.Wire
import Control.Wire.FRP
import Data.Fixed
import Prelude hiding ((.), id)
import Text.Printf


wire :: WireP' a String
wire = loop (cycle $ words "this is a test")
    where
    loop (txt:txts) =
        switch $ proc _ -> do
            swEv <- at [1] -< loop txts
            id -< (txt, swEv)


main :: IO ()
main =
    -- let tl = Tl.insert 6 10 .
    --          Tl.insert 4 20 $
    --          Tl.singleton 2 10
    -- in print (Tl.linAvg 0 100 tl)
    testWire_ 0 clockSession_ wire
