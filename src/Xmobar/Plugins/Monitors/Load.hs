-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Load
-- Copyright   :  Finn Lawler
-- License     :  BSD-style (see LICENSE)
--
-- Author      :  Finn Lawler <flawler@cs.tcd.ie>
-- Maintainer  :  jao <mail@jao.io>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A load average monitor for Xmobar.  Adapted from
-- Xmobar.Plugins.Monitors.Thermal by Juraj Hercek.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Load (loadConfig, runLoad) where

import Xmobar.Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import System.Posix.Files (fileExist)
import Control.Monad (zipWithM)

-- | Default configuration.
loadConfig :: IO MConfig
loadConfig = mkMConfig
       "Load: <load1>" -- template
       ["load1", "load5", "load15"]       -- available replacements

-- | Parses the contents of a loadavg proc file, returning
-- the list of load averages
parseLoadAvgs :: B.ByteString -> [Float]
parseLoadAvgs =
  map ((read :: String -> Float) . B.unpack) . take 3 . B.words . head . B.lines

-- | Retrieves load information.  Returns the monitor string parsed
-- according to template (either default or user specified).
runLoad :: [String] -> Monitor String
runLoad _ = do
  let file = "/proc/loadavg"
  exists <- io $ fileExist file
  if exists then
      (do l <- io $ B.readFile file >>= return . parseLoadAvgs
          d <- getConfigValue decDigits
          let s = showWithColors . const . showDigits d
          parseTemplate =<< zipWithM s l l)
    else
      return "Load: N/A"
