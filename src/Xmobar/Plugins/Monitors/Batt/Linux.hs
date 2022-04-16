-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt.Linux
-- Copyright   :  (c) 2010, 2011, 2012, 2013, 2015, 2016, 2018, 2019 Jose A Ortega
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

import Xmobar.Plugins.Monitors.Batt.Common (BattOpts(..)
                                           , Result(..)
                                           , Status(..)
                                           , maybeAlert)

import Control.Monad (unless)
import Control.Exception (SomeException, handle)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hGetLine, withFile, Handle)
import System.Posix.Files (fileExist)
import Data.List (sort, sortBy, group)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Text.Read (readMaybe)

data Files = Files
  { fFull :: String
  , fNow :: String
  , fVoltage :: String
  , fVoltageMin :: String
  , fCurrent :: Maybe String
  , fPower :: Maybe String
  , fStatus :: String
  } | NoFiles deriving Eq

data Battery = Battery
  { full :: !Float
  , now :: !Float
  , power :: !Float
  , status :: !String
  }

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

safeFileExist :: String -> String -> IO Bool
safeFileExist d f = handle noErrors $ fileExist (d </> f)
  where noErrors = const (return False) :: SomeException -> IO Bool

batteryFiles :: String -> IO Files
batteryFiles bat =
  do is_charge <- exists "charge_now"
     is_energy <- if is_charge then return False else exists "energy_now"
     plain <- exists (if is_charge then "charge_full" else "energy_full")
     has_power <- exists powerNowPath
     has_current <- exists currentNowPath
     let pf = if has_power then Just powerNowPath else Nothing
         cf = if has_current then Just currentNowPath else Nothing
         sf = if plain then "" else "_design"
     return $ case (is_charge, is_energy) of
       (True, _) -> files "charge" cf pf sf 
       (_, True) -> files "energy" cf pf sf
       _ -> NoFiles
  where prefix = sysDir </> bat
        justPrefix a = Just $ prefix </> a
        exists = safeFileExist prefix
        files ch cf pf sf = Files { fFull = prefix </> ch ++ "_full" ++ sf
                                  , fNow = prefix </> ch ++ "_now"
                                  , fCurrent = maybe Nothing justPrefix cf
                                  , fPower = maybe Nothing justPrefix pf
                                  , fVoltage = prefix </> "voltage_now"
                                  , fVoltageMin = prefix </> "voltage_min_design"
                                  , fStatus = prefix </> "status"}
        currentNowPath = "current_now"
        powerNowPath = "power_now"

haveAc :: FilePath -> IO Bool
haveAc f =
  handle (onError False) $ withFile (sysDir </> f) ReadMode (fmap (== "1") . hGetLine)

readBatPower :: Float -> Files -> IO Float

readBatPower sc (Files {fPower = Just p}) = 
    do valp <- grabNumber p
       return $ valp / sc

readBatPower sc (Files {fPower = Nothing, fCurrent = Just c, fVoltage = v}) = 
    do valv <- grabNumber v
       valc <- grabNumber c
       return $ valc * valv / (sc * sc)

readBatPower _ _ = do return 999

readBattery :: Float -> Files -> IO Battery
readBattery _ NoFiles = return $ Battery 0 0 0 "Unknown"
readBattery sc files =
    do a <- grabNumber $ fFull files
       b <- grabNumber $ fNow files
       p <- readBatPower sc files
       s <- grabString $ fStatus files
       let a' = max a b -- sometimes the reported max charge is lower than
       return $ Battery (3600 * a' / sc) -- wattseconds
                        (3600 * b / sc) -- wattseconds
                        (abs p) -- watts
                        s -- string: Discharging/Charging/Full

grabNumber :: (Num a, Read a) => FilePath -> IO a
grabNumber = grabFile (-1) (fmap read . hGetLine)

grabString :: FilePath -> IO String
grabString = grabFile "Unknown" hGetLine

grabFile :: a -> (Handle -> IO a) -> FilePath -> IO a
grabFile returnOnError readMode f = handle (onFileError returnOnError) $ withFile f ReadMode readMode

onFileError :: a -> SomeException -> IO a
onFileError returnOnError = const (return returnOnError) 

-- sortOn is only available starting at ghc 7.10
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

mostCommonDef :: Eq a => a -> [a] -> a
mostCommonDef x xs = head $ last $ [x] : sortOn length (group xs)

readBatteries :: BattOpts -> [String] -> IO Result
readBatteries opts bfs =
    do bfs' <- mapM batteryFiles bfs
       let bfs'' = filter (/= NoFiles) bfs'
       bats <- mapM (readBattery (scale opts)) (take 3 bfs'')
       ac <- haveAc (onlineFile opts)
       let sign = if ac then 1 else -1
           ft = sum (map full bats)
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
