-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Load.Common
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

module Xmobar.Plugins.Monitors.Load.Common (
  Result(..)
  ) where

data Result = Result [Float] | NA
