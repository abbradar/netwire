-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Monad.Fix
import Control.Wire
import Prelude hiding ((.), id)


netwireIsCool :: SimpleWire a String
netwireIsCool =
    for 2 . "Once upon a time..." -->
    for 3 . "... games were completely imperative..." -->
    for 2 . "... but then..." -->
    for 10 . ("Netwire 5! " <>
              (holdFor 0.5 . (now <& periodic 1) . "Hoo..." <|>
               "...ray!")) -->
    netwireIsCool


main :: IO ()
main = testWire clockSession_ netwireIsCool
