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

module Xmobar.Text.Swaybar (prepare, formatSwaybar) where

import Data.Aeson

import Data.ByteString.Lazy.UTF8 (toString)

import GHC.Generics

import Xmobar.Config.Types (Config)

import Xmobar.Run.Parsers ( Segment
                          , Widget(..)
                          , Box(..)
                          , BoxBorder(..)
                          , tBoxes
                          , tColorsString
                          , colorComponents)

import Xmobar.Text.SwaybarClicks (startHandler)

data Preamble =
  Preamble {version :: !Int, click_events :: Bool} deriving (Eq,Show,Generic)

asString :: ToJSON a => a -> String
asString = toString . encode

preamble :: String
preamble = (asString $ Preamble { version = 1, click_events = True }) ++ "\x0A["

data Block =
  Block { full_text :: String
        , name :: String
        , color :: Maybe String
        , background :: Maybe String
        , separator :: Bool
        , separator_block_width :: Int
        , border :: Maybe String
        , border_top :: Maybe Int
        , border_bottom :: Maybe Int
        , border_left :: Maybe Int
        , border_right :: Maybe Int
        } deriving (Eq,Show,Generic)


defaultBlock :: Block
defaultBlock = Block { full_text = ""
                     , name = ""
                     , color = Nothing
                     , background = Nothing
                     , separator = False
                     , separator_block_width = 0
                     , border = Nothing
                     , border_top = Nothing
                     , border_bottom = Nothing
                     , border_left = Nothing
                     , border_right = Nothing
                     }

instance ToJSON Block where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance ToJSON Preamble

withBox :: Box -> Block -> Block
withBox (Box b _ n c _) bl =
  (case b of
     BBFull -> bl { border_right = w, border_left = w
                  , border_bottom = w, border_top = w  }
     BBTop -> bl { border_top = w }
     BBBottom -> bl { border_bottom = w }
     BBVBoth -> bl { border_bottom = w, border_top = w }
     BBLeft -> bl { border_left = w }
     BBRight -> bl { border_right = w }
     BBHBoth -> bl { border_right = w, border_left = w }
  ) { border = bc }
  where w = Just (fromIntegral n)
        bc = if null c then Nothing else Just c

formatSwaybar' :: Config -> Segment -> Block
formatSwaybar' conf (Text txt, info, _, as) =
  foldr withBox block (tBoxes info)
  where (fg, bg) = colorComponents conf (tColorsString info)
        block = defaultBlock { full_text = txt
                             , color = Just fg
                             , background = Just bg
                             , name = show as
                             }
formatSwaybar' conf (Hspace n, info, i, a) =
  formatSwaybar' conf (Text (replicate (fromIntegral n) ' '), info, i, a)
formatSwaybar' _ _ = defaultBlock

collectBlock :: Block -> [Block] -> [Block]
collectBlock b [] = [b]
collectBlock b (h:bs) =
  if b {full_text = ""} == h {full_text = ""} then
    h {full_text = full_text b ++ full_text h} : bs
  else b:h:bs

collectSegment :: Config -> Segment -> [Block] -> [Block]
collectSegment config segment blocks =
  if null $ full_text b then blocks else collectBlock b blocks
  where b = formatSwaybar' config segment

formatSwaybar :: Config -> [Segment] -> String
formatSwaybar conf segs = asString blocks ++ ","
  where blocks = foldr (collectSegment conf) [] segs

prepare :: IO ()
prepare = startHandler >> putStrLn preamble
