------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.TextEventLoop
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

module Xmobar.App.TextEventLoop (startTextLoop) where

import Prelude hiding (lookup)

import Control.Monad.Reader

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM

import Xmobar.System.Signal
import Xmobar.Config.Types (Config)
import Xmobar.X11.Parsers (Segment, Widget(..), parseString)
import Xmobar.App.CommandThreads (initLoop)

-- | Starts the main event loop and threads
startTextLoop :: Config
              -> TMVar SignalType
              -> TMVar ()
              -> [[([Async ()], TVar String)]]
              -> IO ()
startTextLoop cfg sig pauser vs = do
    tv <- initLoop sig pauser vs
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

asText :: Segment -> String
asText (Text s, _, _, _) = s
asText (Hspace n, _, _, _) = replicate (fromIntegral n) ' '
asText _ = ""

parseStringAsText :: Config -> String -> IO String
parseStringAsText c s = do
  segments <- parseString c s
  let txts = map asText segments
  return (concat txts)
