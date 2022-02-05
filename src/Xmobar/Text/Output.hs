-- |
-- Module: Xmobar.Text.Output
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Feb 4, 2022 01:10
--
--
-- Format strings emitted by Commands into output strings
--
------------------------------------------------------------------------------

module Xmobar.Text.Output (initLoop, format) where

import Xmobar.Config.Types (Config(textOutputFormat), TextOutputFormat(..))
import Xmobar.Run.Parsers ( Segment
                          , Widget(..)
                          , parseString
                          , tColorsString
                          , colorComponents)

import Xmobar.Text.Ansi (withAnsiColor)
import Xmobar.Text.Pango (withPangoColor)
import Xmobar.Text.Swaybar (formatSwaybar, prepare)

initLoop :: Config -> IO ()
initLoop conf = case textOutputFormat conf of
  Swaybar -> prepare
  _ -> return ()

withColor :: TextOutputFormat -> (String, String) -> String -> String
withColor Ansi c = withAnsiColor c
withColor Pango c = withPangoColor c
withColor _ _ = id

formatWithColor :: Config -> Segment -> String
formatWithColor conf (Text s, info, _, _) =
  withColor (textOutputFormat conf) components s
  where components = colorComponents conf color
        color = tColorsString info
formatWithColor conf (Hspace n, i, x, y) =
   formatWithColor conf (Text $ replicate (fromIntegral n) ' ', i, x, y)
formatWithColor _ _ = ""

format :: Config -> String -> IO String
format conf s = do
  segments <- parseString conf s
  case textOutputFormat conf of
    Swaybar -> return $ formatSwaybar conf segments
    _ -> return (concatMap (formatWithColor conf) segments)
