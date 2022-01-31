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

module Xmobar.App.TextEventLoop (textLoop) where

import Prelude hiding (lookup)
import System.IO
import Data.List (intercalate)

import Control.Monad.Reader

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM

import Xmobar.System.Signal
import Xmobar.Config.Types (Config, ansiColors)
import Xmobar.X11.Parsers (Segment, Widget(..), parseString, tColorsString, colorComponents)
import Xmobar.App.CommandThreads (initLoop, loop)

-- | Starts the main event loop and threads
textLoop :: Config -> IO ()
textLoop conf = loop conf (startTextLoop' conf)

startTextLoop' :: Config
               -> TMVar SignalType
               -> TMVar ()
               -> [[([Async ()], TVar String)]]
               -> IO ()
startTextLoop' cfg sig pauser vs = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
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

asInt :: String -> String
asInt x = case (reads $ "0x" ++ x)  :: [(Integer, String)] of
  [(v, "") ] -> show v
  _ -> ""

namedColor :: String -> String
namedColor c =
  case c of
    "black" -> "0"; "red" -> "1"; "green" -> "2"; "yellow" -> "3"; "blue" -> "4";
    "magenta" -> "5"; "cyan" -> "6"; "white" -> "7"; _ -> ""

ansiCode :: String -> String
ansiCode ('#':r:g:[b]) = ansiCode ['#', '0', r, '0', g, '0', b]
ansiCode ('#':r0:r1:g0:g1:b0:[b1]) =
  "2;" ++ intercalate ";" (map asInt [[r0,r1], [g0,g1], [b0,b1]])
ansiCode ('#':n) = ansiCode n
ansiCode c = "5;" ++ if null i then namedColor c else i where i = asInt c

withAnsiColor :: (String, String) -> String -> String
withAnsiColor (fg, bg) s = wrap "38;" fg (wrap "48;" bg s)
  where wrap cd cl w =
          if null cl
          then w
          else "\x1b[" ++ cd ++ ansiCode cl ++ "m" ++ w ++ "\x1b[0m"

asText :: Config -> Segment -> String
asText conf (Text s, info, _, _) =
  if null color then s else withAnsiColor (colorComponents conf color) s
  where color = if ansiColors conf then tColorsString info else ""
asText colors (Hspace n, i, x, y) =
  asText colors (Text $ replicate (fromIntegral n) ' ', i, x, y)
asText _ _ = ""

parseStringAsText :: Config -> String -> IO String
parseStringAsText c s = do
  segments <- parseString c s
  let txts = map (asText c) segments
  return (concat txts)
