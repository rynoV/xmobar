------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.CommandThreads
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Fri Jan 28, 2022 03:20
--
--
-- Running a thread for each defined Command
--
------------------------------------------------------------------------------

module Xmobar.App.CommandThreads ( startCommand
                                 , newRefreshLock
                                 , refreshLock
                                 , refreshLockT) where

import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async)
import Control.Exception (bracket_)

import Xmobar.System.Signal (SignalType)
import Xmobar.Run.Runnable (Runnable)
import Xmobar.Run.Exec (start, trigger, alias)

newRefreshLock :: IO (TMVar ())
newRefreshLock = newTMVarIO ()

refreshLock :: TMVar () -> IO a -> IO a
refreshLock var = bracket_ lock unlock
    where
        lock = atomically $ takeTMVar var
        unlock = atomically $ putTMVar var ()

refreshLockT :: TMVar () -> STM a -> STM a
refreshLockT var action = do
    takeTMVar var
    r <- action
    putTMVar var ()
    return r

-- | Runs a command as an independent thread and returns its Async handles
-- and the TVar the command will be writing to.
startCommand :: TMVar SignalType
             -> (Runnable,String,String)
             -> IO ([Async ()], TVar String)
startCommand sig (com,s,ss)
    | alias com == "" = do var <- newTVarIO is
                           atomically $ writeTVar var (s ++ ss)
                           return ([], var)
    | otherwise = do var <- newTVarIO is
                     let cb str = atomically $ writeTVar var (s ++ str ++ ss)
                     a1 <- async $ start com cb
                     a2 <- async $ trigger com $ maybe (return ())
                                                 (atomically . putTMVar sig)
                     return ([a1, a2], var)
    where is = s ++ "Updating..." ++ ss
