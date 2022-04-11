{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Load.FreeBSD
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

module Xmobar.Plugins.Monitors.Load.FreeBSD (fetchLoads) where

import Xmobar.Plugins.Monitors.Load.Common (Result(..))
import Foreign.C.Types (CUInt, CUIntMax)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable, alignment, peek, peekByteOff, poke, sizeOf)
import System.BSD.Sysctl (sysctlPeek)

#include <sys/resource.h>


data LoadAvg = LoadAvg {loads :: [Float]}


calcLoad :: CUInt -> CUIntMax -> Float
calcLoad l s = ((fromIntegral . toInteger) l) / ((fromIntegral . toInteger) s)


instance Storable LoadAvg where
  alignment _ = #{alignment struct loadavg}
  sizeOf _    = #{size struct loadavg}
  peek ptr    = do
    load_values <- peekArray 3 $ #{ptr struct loadavg, ldavg} ptr  :: IO [CUInt]
    scale <- #{peek struct loadavg, fscale} ptr :: IO CUIntMax
    let
      l1 = calcLoad (load_values !! 0) scale
      l5 = calcLoad (load_values !! 1) scale
      l15 = calcLoad (load_values !! 2) scale

    return $ LoadAvg{loads = [l1, l5, l15]}

  poke _ _    = pure ()


fetchLoads :: IO Result
fetchLoads = do
  res <- sysctlPeek "vm.loadavg" :: IO LoadAvg
  return $ Result (loads res)
