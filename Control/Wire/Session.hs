-- |
-- Module:     Control.Wire.Session
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Session
    ( -- * Run wire sessions
      reactimate,

      -- * Test wires
      testWire,
      testWire_
    )
    where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Wire.State
import Control.Wire.Wire
import Data.Data
import System.IO


-- | Quit after this iteration?

data Quit = Quit | NoQuit
    deriving (Data, Eq, Ord, Read, Show, Typeable)


-- | Run the given wire session.

reactimate ::
    (Monad m)
    => m a                -- ^ Input generator.
    -> (Either e b -> m Quit)  -- ^ Process output.
    -> Session m ds       -- ^ State delta generator.
    -> Wire ds e m a b    -- ^ Wire to run.
    -> m ()
reactimate getInput process = loop
    where
    loop s' w' = do
        x' <- getInput
        (ds, s) <- stepSession s'
        (mx, w) <- stepWire w' ds (Right x')
        q <- process mx
        case q of
          Quit   -> return ()
          NoQuit -> loop s w


-- | Test the given wire.  Press Ctrl-C to abort.

testWire ::
    (MonadIO m, Show b, Show e)
    => Int  -- ^ Show output every this number of iterations.
    -> m a  -- ^ Input generator.
    -> Session m ds     -- ^ State delta generator.
    -> Wire ds e m a b  -- ^ Wire to run.
    -> m c
testWire n getInput = loop 0
    where
    loop i s' w' = do
        x' <- getInput
        (ds, s) <- stepSession s'
        (mx, w) <- stepWire w' ds (Right x')
        if i < n
          then mx `seq` loop (succ i) s w
          else do
              liftIO $ do
                  putChar '\r'
                  putStr (either (("I:" ++ ) . show) show mx)
                  putStr "\27[K"
                  hFlush stdout
              loop 0 s w


-- | Simplified interface to 'testWire' for wires that ignore their
-- input and use a simple state delta generator.

testWire_ ::
    (Applicative m, MonadIO m, Show b, Show e)
    => Int  -- ^ Show output every this number of iterations.
    -> Session m ds            -- ^ State delta generator.
    -> (forall a. Wire ds e m a b)  -- ^ Wire to run.
    -> m c
testWire_ n s w = testWire n (return ()) s w
