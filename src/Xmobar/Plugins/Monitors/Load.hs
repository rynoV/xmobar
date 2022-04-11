{-# LANGUAGE CPP #-}

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
import Xmobar.Plugins.Monitors.Load.Common (Result(..))

#if defined(freebsd_HOST_OS)
import qualified Xmobar.Plugins.Monitors.Load.FreeBSD as ML
#else
import qualified Xmobar.Plugins.Monitors.Load.Linux as ML
#endif


-- | Default configuration.
loadConfig :: IO MConfig
loadConfig = mkMConfig "Load: <load1>" ["load1", "load5", "load15"]


-- | Retrieves load information.  Returns the monitor string parsed
-- according to template (either default or user specified).
runLoad :: [String] -> Monitor String
runLoad _ = do
  result <- io ML.fetchLoads
  case result of
    Result loads ->
      do
        d <- getConfigValue decDigits
        parseTemplate =<< mapM (showWithColors (showDigits d)) loads
    NA -> getConfigValue naString
