{-# LANGUAGE DeriveGeneric #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Text.SwaybarClicks
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Feb 4, 2022 03:58
--
--
-- Handling of "click" events sent by swaybar via stdin
--
------------------------------------------------------------------------------


module Xmobar.Text.SwaybarClicks (startHandler) where

import Control.Monad (when)


import Data.Aeson
import GHC.Generics

import Xmobar.System.Utils (forkThread)
import Xmobar.Run.Actions (Action (..), runAction')

import Data.ByteString.Lazy.UTF8 (fromString)

data Click =
  Click { name :: String , button :: Int } deriving (Eq,Show,Generic)

instance FromJSON Click

runClickAction :: Int -> Action -> IO ()
runClickAction b a@(Spawn bs _) =
  when (fromIntegral b `elem` bs) (runAction' a)

handleClick :: Maybe Click -> IO ()
handleClick Nothing = return ()
handleClick (Just click) = do
  let mas = read (name click) :: Maybe [Action]
      b = button click
  maybe (return ()) (mapM_ (runClickAction b)) mas

toClick :: String -> Maybe Click
toClick (',':s) = toClick s
toClick s = decode (fromString s)

readClicks :: IO ()
readClicks = getLine >>= handleClick . toClick >> readClicks

startHandler :: IO ()
startHandler = forkThread "Swaybar event handler" readClicks
