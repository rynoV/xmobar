------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Text.Swaybar
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Feb 4, 2022 03:58
--
--
-- Segment codification using swaybar-protocol JSON strings
--
------------------------------------------------------------------------------

module Xmobar.Text.Swaybar (preamble, formatSwaybar) where

import Data.List (intercalate)

import Xmobar.Config.Types (Config)

import Xmobar.Run.Parsers ( Segment
                          , Widget(..)
                          -- , tColorsString
                          -- , colorComponents
                          )

preamble :: String
preamble = "{\"version\": 1, \"click_events\": true}\x0A["

formatSwaybar' :: Config -> Segment -> String
formatSwaybar' _conf (Text txt, _, _, _) =
  "{\"full_text\":\"" ++ txt ++ "\"}"
formatSwaybar' _ _ = ""

formatSwaybar :: Config -> [Segment] -> String
formatSwaybar conf segs =
  "[" ++ intercalate "," (map (formatSwaybar' conf) segs) ++ "],"
