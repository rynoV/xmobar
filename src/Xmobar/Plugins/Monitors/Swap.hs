{-#LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Swap
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A  swap usage monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Swap where

import Xmobar.Plugins.Monitors.Common

#if defined(freebsd_HOST_OS)
import qualified Xmobar.Plugins.Monitors.Swap.FreeBSD as MS
#else
import qualified Xmobar.Plugins.Monitors.Swap.Linux as MS
#endif

swapConfig :: IO MConfig
swapConfig = mkMConfig "Swap: <usedratio>%"
                       ["usedratio", "total", "used", "free"]

formatSwap :: [Float] -> Monitor [String]
formatSwap (r:xs) = do
  d <- getConfigValue decDigits
  other <- mapM (showWithColors (showDigits d)) xs
  ratio <- showPercentWithColors r
  return $ ratio:other
formatSwap _ = replicate 4 `fmap` getConfigValue naString

runSwap :: [String] -> Monitor String
runSwap _ =
    do m <- io MS.parseMEM
       l <- formatSwap m
       parseTemplate l
