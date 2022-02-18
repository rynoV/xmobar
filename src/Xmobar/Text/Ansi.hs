------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Text.Ansi
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Feb 4, 2022 01:10
--
--
-- Codification with ANSI (color) escape codes
--
------------------------------------------------------------------------------

module Xmobar.Text.Ansi (withAnsiColor) where

import Data.List (intercalate)
import Data.Char (toLower)

asInt :: String -> String
asInt x = case (reads $ "0x" ++ x)  :: [(Integer, String)] of
  [(v, "") ] -> show v
  _ -> ""

namedColor :: String -> String
namedColor c =
  case map toLower c of
    "black" -> "0"; "red" -> "1"; "green" -> "2"; "yellow" -> "3"; "blue" -> "4";
    "magenta" -> "5"; "cyan" -> "6"; "white" -> "7";
    "brightblack" -> "8"; "brightred" -> "9"; "brightgreen" -> "10";
    "brightyellow" -> "11"; "brightblue" -> "12";
    "brightmagenta" -> "13"; "brightcyan" -> "14"; "brightwhite" -> "15";
    _ -> ""

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
