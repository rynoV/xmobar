{-# LANGUAGE DeriveGeneric #-}

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

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)

import GHC.Generics

import Xmobar.Config.Types (Config)

import Xmobar.Run.Parsers ( Segment
                          , Widget(..)
                          , tColorsString
                          , colorComponents
                          )

data Preamble =
  Preamble {version :: !Int, click_events :: Bool} deriving (Eq,Show,Generic)

asString :: ToJSON a => a -> String
asString = toString . encode

preamble :: String
preamble = (asString $ Preamble { version = 1, click_events = True }) ++ "\x0A["

data Block =
  Block { full_text :: !String
        , color :: !String
        , background :: !String
        , separator :: !Bool
        , separator_block_width :: !Int
        , name :: !String
        } deriving (Eq,Show,Generic)

defaultBlock :: Block
defaultBlock = Block { full_text = ""
                     , name = ""
                     , color = ""
                     , background = ""
                     , separator = False
                     , separator_block_width = 0}

instance ToJSON Preamble
instance ToJSON Block

formatSwaybar' :: Config -> Segment -> Block
formatSwaybar' conf (Text txt, info, _, as) =
  defaultBlock {full_text = txt , color = fg , background = bg , name = show as}
  where (fg, bg) = colorComponents conf (tColorsString info)
formatSwaybar' conf (Hspace n, info, i, a) =
  formatSwaybar' conf (Text (replicate (fromIntegral n) ' '), info, i, a)
formatSwaybar' _ _ = defaultBlock

formatSwaybar :: Config -> [Segment] -> String
formatSwaybar conf segs = asString elems ++ ","
  where elems = filter (not . null . full_text) (map (formatSwaybar' conf) segs)
