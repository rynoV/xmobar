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
-- Format segments emitted by Commands into output strings
--
------------------------------------------------------------------------------

module Xmobar.Text.Output (formatSegment) where

import Xmobar.Config.Types (Config(textOutputFormat), TextOutputFormat(..))
import Xmobar.Run.Parsers ( Segment
                          , Widget(..)
                          , tColorsString
                          , colorComponents)

import Xmobar.Text.Ansi (withAnsiColor)
import Xmobar.Text.Pango (withPangoColor)

withColor :: TextOutputFormat -> (String, String) -> String -> String
withColor format color = case format of
                           Plain -> id
                           Ansi -> withAnsiColor color
                           Pango -> withPangoColor color


formatSegment :: Config -> Segment -> String
formatSegment conf (Text s, info, _, _) =
  withColor (textOutputFormat conf) components s
  where components = colorComponents conf color
        color = tColorsString info
formatSegment conf (Hspace n, i, x, y) =
   formatSegment conf (Text $ replicate (fromIntegral n) ' ', i, x, y)
formatSegment _ _ = ""
