------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.Main
-- Copyright: (c) 2018, 2019, 2020, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 21:53
--
--
-- Support for creating executable main functions
--
------------------------------------------------------------------------------


module Xmobar.App.Main (xmobar, xmobarMain, configFromArgs) where


import Data.List (intercalate)
import System.Posix.Process (executeFile)
import System.Environment (getArgs)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension)
import Text.Parsec.Error (ParseError)

import Control.Monad (unless)

import Xmobar.App.Config
import Xmobar.Config.Types
import Xmobar.Config.Parse
import Xmobar.X11.Loop (x11Loop)
import Xmobar.Text.Loop (textLoop)
import Xmobar.App.Opts (recompileFlag, verboseFlag, getOpts, doOpts)
import Xmobar.App.Compile (recompile, trace)

xmobar :: Config -> IO ()
xmobar cfg = if textOutput cfg then textLoop cfg else x11Loop cfg

configFromArgs :: Config -> IO Config
configFromArgs cfg = getArgs >>= getOpts >>= doOpts cfg . fst

buildLaunch :: [String] -> Bool -> Bool -> String -> ParseError -> IO ()
buildLaunch args verb force p e = do
  let exec = takeBaseName p
      confDir = takeDirectory p
      ext = takeExtension p
  if ext `elem` [".hs", ".hsc", ".lhs"]
    then xmobarDataDir >>= \dd -> recompile confDir dd exec force verb >>
         executeFile (confDir </> exec) False args Nothing
    else trace True ("Invalid configuration file: " ++ show e) >>
         trace True "\n(No compilation attempted: \
                    \only .hs, .hsc or .lhs files are compiled)"

xmobar' :: [String] -> Config -> IO ()
xmobar' defs cfg = do
  unless (null defs || not (verbose cfg)) $ putStrLn $
    "Fields missing from config defaulted: " ++ intercalate "," defs
  xmobar cfg

xmobarMain :: IO ()
xmobarMain = do
  args <- getArgs
  (flags, rest) <- getOpts args
  cf <- case rest of
          [c] -> return (Just c)
          [] -> xmobarConfigFile
          _ -> error $ "Too many arguments: " ++ show rest
  case cf of
    Nothing -> case rest of
                (c:_) -> error $ c ++ ": file not found"
                _ -> doOpts defaultConfig flags >>= xmobar
    Just p -> do r <- readConfig defaultConfig p
                 case r of
                   Left e ->
                     buildLaunch (filter (/= p) args) (verboseFlag flags)
                                 (recompileFlag flags) p e
                   Right (c, defs) -> doOpts c flags >>= xmobar' defs
