-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Load.Linux
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

module Xmobar.Plugins.Monitors.Load.Linux (fetchLoads) where

import Xmobar.Plugins.Monitors.Load.Common (Result(..))
import qualified Data.ByteString.Lazy.Char8 as B
import System.Posix.Files (fileExist)

-- | Parses the contents of a loadavg proc file, returning
-- the list of load averages
parseLoadAvgs :: B.ByteString -> Result
parseLoadAvgs =
  Result . map (read . B.unpack) . take 3 . B.words . head . B.lines

fetchLoads :: IO Result
fetchLoads = do
  let file = "/proc/loadavg"

  exists <- fileExist file
  if exists then
    parseLoadAvgs <$> B.readFile file
    else
    return NA
