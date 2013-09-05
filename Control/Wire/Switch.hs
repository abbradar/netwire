-- |
-- Module:     Control.Wire.Switch
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Switch
    ( -- * Simple switching
      (-->),

      -- * Context-based
      modes
    )
    where

import qualified Data.Map as M
import Control.Wire.Core
import Control.Wire.Event


-- | Acts like the first wire until it inhibits, then switches to the
-- second wire.  Infixr 1.
--
-- * Depends: like current wire.
--
-- * Inhibits: after switching like the second wire.
--
-- * Switch: now.

(-->) :: (Monad m) => Wire s e m a b -> Wire s e m a b -> Wire s e m a b
w1' --> w2' =
    WGen $ \ds mx' -> do
        (mx, w1) <- stepWire w1' ds mx'
        case mx of
          Left _  -> stepWire w2' ds mx'
          Right _ -> mx `seq` return (mx, w1 --> w2')

infixr 1 -->


-- | Route the left input signal based on the current mode.  The right
-- input signal can be used to change the current mode.  When switching
-- away from a mode and then switching back to it, it will be resumed.
-- Freezes time during inactivity.
--
-- * Complexity: O(n * log n) space, O(log n) lookup time on switch wrt
--   number of started, inactive modes.
--
-- * Depends: like currently active wire (left), now (right).
--
-- * Inhibits: when active wire inhibits.
--
-- * Switch: now on mode change.

modes ::
    (Monad m, Ord k)
    => k  -- ^ Initial mode.
    -> (k -> Wire s e m a b)  -- ^ Select wire for given mode.
    -> Wire s e m (a, Event k) b
modes m0 select = loop M.empty m0 (select m0)
    where
    loop ms' m' w'' =
        WGen $ \ds mxev' ->
            case mxev' of
              Left _ -> do
                  (mx, w) <- stepWire w'' ds (fmap fst mxev')
                  return (mx, loop ms' m' w)
              Right (x', ev) -> do
                  let (ms, m, w') = switch ms' m' w'' ev
                  (mx, w) <- stepWire w' ds (Right x')
                  return (mx, loop ms m w)

    switch ms' m' w' NoEvent = (ms', m', w')
    switch ms' m' w' (Event m) =
        let ms = M.insert m' w' ms' in
        case M.lookup m ms of
          Nothing -> (ms, m, select m)
          Just w  -> (M.delete m ms, m, w)
