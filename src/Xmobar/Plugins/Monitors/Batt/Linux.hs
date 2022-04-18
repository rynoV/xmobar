-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt.Linux
-- Copyright   :  (c) 2010-2013, 2015, 2016, 2018, 2019, 2022 Jose A Ortega
--                (c) 2010 Andrea Rossato, Petr Rockai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A battery monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Batt.Linux (readBatteries) where

import Xmobar.Plugins.Monitors.Batt.Common ( BattOpts(..)
                                           , Result(..)
                                           , Status(..)
                                           , maybeAlert)

import Control.Monad (unless)
import Control.Exception (SomeException, handle)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hGetLine, withFile, Handle)
import Data.List (sort, sortBy, group)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Text.Read (readMaybe)

data Files = Files
  { fEFull :: String
  , fCFull :: String
  , fEFullDesign :: String
  , fCFullDesign :: String
  , fENow :: String
  , fCNow :: String
  , fVoltage :: String
  , fVoltageMin :: String
  , fCurrent :: String
  , fPower :: String
  , fStatus :: String
  , fBat :: String
  } deriving Eq

-- the default basenames of the possibly available attributes exposed
-- by the kernel
defaultFiles :: Files
defaultFiles = Files
  { fEFull = "energy_full"
  , fCFull = "charge_full"
  , fEFullDesign = "energy_full_design"
  , fCFullDesign = "charge_full_design"
  , fENow = "energy_now"
  , fCNow = "charge_now"
  , fVoltage = "voltage_now"
  , fVoltageMin = "voltage_min_design"
  , fCurrent = "current_now"
  , fPower = "power_now"
  , fStatus = "status"
  , fBat = "BAT0"
  }

type FilesAccessor = Files -> String

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

battFile :: FilesAccessor -> Files -> FilePath
battFile accessor files = sysDir </> fBat files </> accessor files

grabNumber :: (Num a, Read a) => FilesAccessor -> Files -> IO (Maybe a)
grabNumber = grabFile (fmap read . hGetLine)

grabString :: FilesAccessor -> Files -> IO (Maybe String)
grabString = grabFile hGetLine

-- grab file contents returning Nothing if the file doesn't exist or
-- any other error occurs
grabFile :: (Handle -> IO a) -> FilesAccessor -> Files -> IO (Maybe a)
grabFile readMode accessor files =
  handle (onFileError Nothing) (withFile f ReadMode (fmap Just . readMode))
  where f = battFile accessor files

onFileError :: a -> SomeException -> IO a
onFileError returnOnError = const (return returnOnError)

-- get the filenames for a given battery name
batteryFiles :: String -> Files
batteryFiles bat = defaultFiles { fBat = bat }

data Battery = Battery
  { full :: !Float
  , now :: !Float
  , power :: !Float
  , status :: !String
  }

haveAc :: FilePath -> IO Bool
haveAc f =
  handle (onFileError False) $
    withFile (sysDir </> f) ReadMode (fmap (== "1") . hGetLine)

-- retrieve the currently drawn power in Watt
-- sc is a scaling factor which by kernel documentation must be 1e6
readBatPower :: Float -> Files -> IO (Maybe Float)
readBatPower sc f =
    do pM <- grabNumber fPower f
       cM <- grabNumber fCurrent f
       vM <- grabNumber fVoltage f
       return $ case (pM, cM, vM) of
           (Just pVal, _, _) -> Just $ pVal / sc
           (_, Just cVal, Just vVal) -> Just $ cVal * vVal / (sc * sc)
           (_, _, _) -> Nothing

-- retrieve the maximum capacity in Watt hours
-- sc is a scaling factor which by kernel documentation must be 1e6
-- on getting the voltage: using voltage_min_design will probably underestimate
-- the actual energy content of the battery and using voltage_now will probably
-- overestimate it.
readBatCapacityFull :: Float -> Files -> IO (Maybe Float)
readBatCapacityFull sc f =
    do cM  <- grabNumber fCFull f
       eM  <- grabNumber fEFull f
       cdM <- grabNumber fCFullDesign f
       edM <- grabNumber fEFullDesign f
       -- not sure if Voltage or VoltageMin is more accurate and if both
       -- are always available
       vM  <- grabNumber fVoltageMin f
       return $ case (eM, cM, edM, cdM, vM) of
           (Just eVal, _, _, _, _)         -> Just $ eVal        / sc
           (_, Just cVal, _, _, Just vVal) -> Just $ cVal * vVal / (sc * sc)
           (_, _, Just eVal, _, _)         -> Just $ eVal        / sc
           (_, _, _, Just cVal, Just vVal) -> Just $ cVal * vVal / (sc * sc)
           (_, _, _, _, _) -> Nothing

-- retrieve the current capacity in Watt hours
-- sc is a scaling factor which by kernel documentation must be 1e6
-- on getting the voltage: using voltage_min_design will probably underestimate
-- the actual energy content of the battery and using voltage_now will probably
-- overestimate it.
readBatCapacityNow :: Float -> Files -> IO (Maybe Float)
readBatCapacityNow sc f =
    do cM  <- grabNumber fCNow f
       eM  <- grabNumber fENow f
       vM  <- grabNumber fVoltageMin f -- not sure if Voltage or
                                       -- VoltageMin is more accurate
                                       -- and if both are always
                                       -- available
       return $ case (eM, cM, vM) of
           (Just eVal, _, _)         -> Just $ eVal        / sc
           (_, Just cVal, Just vVal) -> Just $ cVal * vVal / (sc * sc)
           (_, _, _) -> Nothing

readBatStatus :: Files -> IO (Maybe String)
readBatStatus = grabString fStatus

-- collect all relevant battery values with defaults of not available
readBattery :: Float -> Files -> IO Battery
readBattery sc files =
    do cFull <- withDef 0 readBatCapacityFull
       cNow <- withDef 0 readBatCapacityNow
       pwr <- withDef 0 readBatPower
       s <- withDef "Unknown" (const readBatStatus)
       let cFull' = max cFull cNow -- sometimes the reported max
                                   -- charge is lower than
       return $ Battery (3600 * cFull') -- wattseconds
                        (3600 * cNow) -- wattseconds
                        (abs pwr) -- watts
                        s -- string: Discharging/Charging/Full
         where withDef d reader = fromMaybe d `fmap` reader sc files

-- sortOn is only available starting at ghc 7.10
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

mostCommonDef :: Eq a => a -> [a] -> a
mostCommonDef x xs = head $ last $ [x] : sortOn length (group xs)

readBatteries :: BattOpts -> [String] -> IO Result
readBatteries opts bfs =
    do let bfs'' = map batteryFiles bfs
       bats <- mapM (readBattery (scale opts)) (take 3 bfs'')
       ac <- haveAc (onlineFile opts)
       let sign = if ac then 1 else -1
           ft = sum (map full bats) -- total capacity when full
           left = if ft > 0 then sum (map now bats) / ft else 0
           watts = sign * sum (map power bats)
           time = if watts == 0 then 0 else max 0 (sum $ map time' bats)
           mwatts = if watts == 0 then 1 else sign * watts
           time' b = (if ac then full b - now b else now b) / mwatts
           statuses :: [Status]
           statuses = map (fromMaybe Unknown . readMaybe)
                          (sort (map status bats))
           acst = mostCommonDef Unknown $ filter (Unknown/=) statuses
           racst | acst /= Unknown = acst
                 | time == 0 = Idle
                 | ac = Charging
                 | otherwise = Discharging
       unless ac (maybeAlert opts left)
       return $ if isNaN left then NA else Result left watts time racst
