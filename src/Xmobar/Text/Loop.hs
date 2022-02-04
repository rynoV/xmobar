------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Text.Loop
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Fri Jan 28, 2022 01:21
--
--
-- Text-only event loop
--
------------------------------------------------------------------------------

module Xmobar.Text.Loop (loop) where

import Prelude hiding (lookup)
import System.IO

import Control.Monad.Reader

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM

import Xmobar.System.Signal

import Xmobar.Config.Types (Config)

import qualified Xmobar.Run.Loop as Loop

import Xmobar.Run.Parsers (parseString)

import Xmobar.Text.Output (formatSegment)

-- | Starts the main event loop and threads
loop :: Config -> IO ()
loop conf = Loop.loop conf (startTextLoop' conf)

startTextLoop' :: Config
               -> TMVar SignalType
               -> TMVar ()
               -> [[([Async ()], TVar String)]]
               -> IO ()
startTextLoop' cfg sig pauser vs = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    tv <- Loop.initLoop sig pauser vs
    eventLoop cfg tv sig

-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: Config -> TVar [String] -> TMVar SignalType -> IO ()
eventLoop cfg tv signal = do
  typ <- atomically $ takeTMVar signal
  case typ of
    Wakeup -> updateString cfg tv >>= putStrLn >> eventLoop cfg tv signal
    _ -> eventLoop cfg tv signal

updateString :: Config -> TVar [String] -> IO String
updateString conf v = do
  s <- readTVarIO v
  let l:c:r:_ = s ++ repeat ""
  liftIO $ concat `fmap` mapM (parseStringAsText conf) [l, c, r]

parseStringAsText :: Config -> String -> IO String
parseStringAsText c s = do
  segments <- parseString c s
  let txts = map (formatSegment c) segments
  return (concat txts)
