-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Main where

import Control.Monad.Fix
import Control.Wire
import Prelude hiding ((.), id)


data Mode = InGame | InMenu  deriving (Eq, Ord)


test :: (HasTime t s, Monad m, Show t) => Wire s () m a String
test =
    proc _ -> do
        ev1 <- at 3 <& at 5 <& at 10 -< InGame
        ev2 <- at 4 <& at 7 -< InMenu
        t <- time -< ()
        modes InMenu mode -< (t, mergeL ev1 ev2)

    where
    mode InMenu = "We're in the menu"
    mode InGame = "We're in the game and the clock is ticking: " <> arr show


main :: IO ()
main = testWire clockSession_ test
