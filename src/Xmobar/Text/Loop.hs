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

module Xmobar.Text.Loop (textLoop) where

import Prelude hiding (lookup)
import System.IO

import Control.Monad.Reader

import Control.Concurrent.STM

import Xmobar.System.Signal
import Xmobar.Config.Types (Config)
import Xmobar.Run.Loop (loop)
import Xmobar.Run.Parsers (parseString)
import Xmobar.Text.Output (formatSegment)

-- | Starts the main event loop and threads
textLoop :: Config -> IO ()
textLoop conf = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  loop conf (eventLoop conf)

-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: Config -> TMVar SignalType -> TVar [String] -> IO ()
eventLoop cfg signal tv = do
  typ <- atomically $ takeTMVar signal
  case typ of
    Wakeup -> updateString cfg tv >>= putStrLn >> eventLoop cfg signal tv
    _ -> eventLoop cfg signal tv

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
