-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.MultiCoreTemp
-- Copyright   :  (c) 2019, 2020 Felix Springer
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Felix Springer <felixspringer149@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A core temperature monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.MultiCoreTemp (startMultiCoreTemp) where

import Xmobar.Plugins.Monitors.Common
import Control.Monad (filterM)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import System.Console.GetOpt
import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        , listDirectory
                        )

-- | Declare Options.
data CTOpts = CTOpts { maxIconPattern :: Maybe IconPattern
                     , avgIconPattern :: Maybe IconPattern
                     , mintemp :: Float
                     , maxtemp :: Float
                     , hwMonitorPath :: Maybe String
                     }

-- | Set default Options.
defaultOpts :: CTOpts
defaultOpts = CTOpts { maxIconPattern = Nothing
                     , avgIconPattern = Nothing
                     , mintemp = 0
                     , maxtemp = 100
                     , hwMonitorPath = Nothing
                     }

-- | Apply configured Options.
options :: [OptDescr (CTOpts -> CTOpts)]
options = [ Option [] ["max-icon-pattern"]
              (ReqArg
                (\ arg opts -> opts { maxIconPattern = Just $ parseIconPattern arg })
                "")
              ""
          , Option [] ["avg-icon-pattern"]
              (ReqArg
                (\ arg opts -> opts { avgIconPattern = Just $ parseIconPattern arg })
                "")
              ""
          , Option [] ["mintemp"]
              (ReqArg
                (\ arg opts -> opts { mintemp = read arg })
                "")
              ""
          , Option [] ["maxtemp"]
              (ReqArg
                (\ arg opts -> opts { maxtemp = read arg })
                "")
              ""
          , Option [] ["hwmon-path"]
              (ReqArg
                (\ arg opts -> opts { hwMonitorPath = Just arg })
                "")
              ""
          ]

-- | Generate Config with a default template and options.
cTConfig :: IO MConfig
cTConfig = mkMConfig cTTemplate cTOptions
  where cTTemplate = "Temp: <max>°C - <maxpc>%"
        cTOptions = [ "max" , "maxpc" , "maxbar" , "maxvbar" , "maxipat"
                    , "avg" , "avgpc" , "avgbar" , "avgvbar" , "avgipat"
                    ] ++ map (("core" ++) . show) [0 :: Int ..]

-- | Returns all paths in dir matching the predicate.
getMatchingPathsInDir :: FilePath -> (String -> Bool) -> IO [FilePath]
getMatchingPathsInDir dir f = do exists <- doesDirectoryExist dir
                                 if exists
                                    then do
                                      files <- filter f <$> listDirectory dir
                                      return $ fmap (\file -> dir ++ "/" ++ file) files
                                    else return []

-- | Given a prefix, suffix, and path string, return true if the path string
-- format is prefix ++ numeric ++ suffix.
numberedPathMatcher :: String -> String -> String -> Bool
numberedPathMatcher prefix suffix path =
    prefix `isPrefixOf` path
    && not (null digits)
    && afterDigits == suffix
  where afterPrefix = drop (length prefix) path
        digits = takeWhile isDigit afterPrefix
        afterDigits = dropWhile isDigit afterPrefix

-- | Returns the first coretemp.N path found.
coretempPath :: IO (Maybe String)
coretempPath = do ps <- getMatchingPathsInDir "/sys/bus/platform/devices" coretempMatcher
                  xs <- filterM doesDirectoryExist ps
                  return (if null xs then Nothing else Just $ head xs ++ "/")
  where coretempMatcher = numberedPathMatcher "coretemp." ""

-- | Returns the first hwmonN in coretemp path found or the ones in sys/class.
hwmonPaths :: IO [String]
hwmonPaths = do p <- coretempPath
                let (sc, path) = case p of
                                   Just s -> (False, s)
                                   Nothing -> (True, "/sys/class/")
                cps <- getMatchingPathsInDir (path ++ "hwmon") hwmonMatcher
                ecps <- filterM doesDirectoryExist cps
                return $ if sc || null ecps then ecps else [head ecps]
  where hwmonMatcher = numberedPathMatcher "hwmon" ""

-- | Checks Labels, if they refer to a core and returns Strings of core-
-- temperatures.
corePaths :: Maybe String -> IO [String]
corePaths s = do ps <- case s of
                        Just pth -> return [pth]
                        _ -> hwmonPaths
                 cps <- concat <$> traverse (`getMatchingPathsInDir` corePathMatcher) ps
                 ls <- filterM doesFileExist cps
                 cls <- filterM isLabelFromCore ls
                 return $ map labelToCore cls
  where corePathMatcher = numberedPathMatcher "temp" "_label"

-- | Checks if Label refers to a core.
isLabelFromCore :: FilePath -> IO Bool
isLabelFromCore p = do a <- readFile p
                       return $ take 4 a `elem` ["Core", "Tdie", "Tctl"]

-- | Transform a path to Label to a path to core-temperature.
labelToCore :: FilePath -> FilePath
labelToCore = (++ "input") . reverse . drop 5 . reverse

-- | Reads core-temperatures as data from the system.
cTData :: Maybe String -> IO [Float]
cTData p = do fps <- corePaths p
              traverse readSingleFile fps
  where readSingleFile :: FilePath -> IO Float
        readSingleFile s = do a <- readFile s
                              return $ parseContent a
          where parseContent :: String -> Float
                parseContent = read . head . lines

-- | Transforms data of temperatures into temperatures of degree Celsius.
parseCT :: CTOpts -> IO [Float]
parseCT opts = do rawCTs <- cTData (hwMonitorPath opts)
                  let normalizedCTs = map (/ 1000) rawCTs :: [Float]
                  return normalizedCTs

-- | Performs calculation for maximum and average.
-- Sets up Bars and Values to be printed.
formatCT :: CTOpts -> [Float] -> Monitor [String]
formatCT opts cTs = do let CTOpts { mintemp = minT
                                  , maxtemp = maxT } = opts
                           domainT = maxT - minT
                           maxCT = maximum cTs
                           avgCT = sum cTs / fromIntegral (length cTs)
                           calcPc t = (t - minT) / domainT
                           maxCTPc = calcPc maxCT
                           avgCTPc = calcPc avgCT

                       cs <- traverse showTempWithColors cTs

                       m <- showTempWithColors maxCT
                       mp <- showWithColors' (show (round (100*maxCTPc) :: Int)) maxCT
                       mb <- showPercentBar maxCT maxCTPc
                       mv <- showVerticalBar maxCT maxCTPc
                       mi <- showIconPattern (maxIconPattern opts) maxCTPc

                       a <- showTempWithColors avgCT
                       ap <- showWithColors' (show (round (100*avgCTPc) :: Int)) avgCT
                       ab <- showPercentBar avgCT avgCTPc
                       av <- showVerticalBar avgCT avgCTPc
                       ai <- showIconPattern (avgIconPattern opts) avgCTPc

                       let ms = [ m , mp , mb , mv , mi ]
                           as = [ a , ap , ab , av , ai ]

                       return (ms ++ as ++ cs)
  where showTempWithColors :: Float -> Monitor String
        showTempWithColors = showWithColors (show . (round :: Float -> Int))


runCT :: [String] -> Monitor String
runCT argv = do opts <- io $ parseOptsWith options defaultOpts argv
                cTs <- io $ parseCT opts
                l <- formatCT opts cTs
                parseTemplate l

startMultiCoreTemp :: [String] -> Int -> (String -> IO ()) -> IO ()
startMultiCoreTemp a = runM a cTConfig runCT
