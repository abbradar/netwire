-- |
-- Module:     Control.Wire.Session
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Wire.Session
    ( -- * Run wire sessions
      reactimate,
      reactimateM,

      -- * Test wires
      testWire,
      testWire_,
      testWireM,
      testWireM_
    )
    where

import Control.Monad.IO.Class
import Control.Wire.State
import Control.Wire.Wire
import Data.Data
import Data.Functor.Identity
import System.IO


-- | Quit after this iteration?

data Quit = Quit | NoQuit
    deriving (Data, Eq, Ord, Read, Show, Typeable)


-- | Run the given wire session.

reactimate ::
    (Monad m)
    => m a                -- ^ Input generator.
    -> (Either e b -> m Quit)  -- ^ Process output.
    -> Session m s        -- ^ State delta generator.
    -> WireP s e a b      -- ^ Wire to run.
    -> m ()
reactimate getInput process s0 w0 =
    reactimateM getInput process (return . runIdentity) s0 w0


-- | Like 'reactimate', but for effectful wires.

reactimateM ::
    (Monad m, Monad m')
    => m a                 -- ^ Input generator.
    -> (Either e b -> m Quit)   -- ^ Process output.
    -> (forall a. m' a -> m a)  -- ^ Monad morphism from the wire monad.
    -> Session m s         -- ^ State delta generator.
    -> Wire s e m' a b     -- ^ Wire to run.
    -> m ()
reactimateM getInput process m = loop
    where
    loop s' w' = do
        x' <- getInput
        (ds, s) <- stepSession s'
        (mx, w) <- m (stepWire w' ds (Right x'))
        q <- process mx
        case q of
          Quit   -> return ()
          NoQuit -> loop s w


-- | Test the given wire.  Press Ctrl-C to abort.

testWire ::
    (MonadIO m, Show b, Show e)
    => Int  -- ^ Show output every this number of iterations.
    -> m a  -- ^ Input generator.
    -> Session m s    -- ^ State delta generator.
    -> WireP s e a b  -- ^ Wire to run.
    -> m c
testWire n getInput s0 w0 =
    testWireM n getInput (return . runIdentity) s0 w0


-- | Simplified interface to 'testWire' for wires that ignore their
-- input.

testWire_ ::
    (MonadIO m, Show b, Show e)
    => Int  -- ^ Show output every this number of iterations.
    -> Session m s           -- ^ State delta generator.
    -> (forall a. WireP s e a b)  -- ^ Wire to run.
    -> m c
testWire_ n s0 w0 =
    testWireM_ n (return . runIdentity) s0 w0


-- | Like 'testWire', but for effectful wires.

testWireM ::
    (Monad m', MonadIO m, Show b, Show e)
    => Int  -- ^ Show output every this number of iterations.
    -> m a  -- ^ Input generator.
    -> (forall a. m' a -> m a)  -- ^ Monad morphism from the wire monad.
    -> Session m s         -- ^ State delta generator.
    -> Wire s e m' a b     -- ^ Wire to run.
    -> m c
testWireM n getInput m = loop 0
    where
    loop i s' w' = do
        x' <- getInput
        (ds, s) <- stepSession s'
        (mx, w) <- m (stepWire w' ds (Right x'))
        if i < n
          then mx `seq` loop (succ i) s w
          else do
              liftIO $ do
                  putChar '\r'
                  putStr (either (("I:" ++ ) . show) show mx)
                  putStr "\27[K"
                  hFlush stdout
              loop 0 s w


-- | Simplified interface to 'testWireM' for wires that ignore their
-- input.

testWireM_ ::
    (Monad m', MonadIO m, Show b, Show e)
    => Int  -- ^ Show output every this number of iterations.
    -> (forall a. m' a -> m a)      -- ^ Monad morphism from the wire monad.
    -> Session m s             -- ^ State delta generator.
    -> (forall a. Wire s e m' a b)  -- ^ Wire to run.
    -> m c
testWireM_ n m s0 w0 = testWireM n (return ()) m s0 w0
