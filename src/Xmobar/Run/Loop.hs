{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Run.Loop
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Fri Jan 28, 2022 03:20
--
--
-- Running a thread for each defined Command in a loop
--
------------------------------------------------------------------------------

module Xmobar.Run.Loop (LoopFunction, loop) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket_, bracket, handle, SomeException(..))
import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad (guard, void, unless)
import Data.Maybe (isJust)
import Data.Foldable (for_)

import Xmobar.System.Signal
import Xmobar.Config.Types
import Xmobar.Run.Runnable (Runnable)
import Xmobar.Run.Exec (start, trigger, alias)
import Xmobar.Run.Template
import Xmobar.Run.Timer (withTimer)

#ifdef DBUS
import Xmobar.System.DBus
#endif

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

type LoopFunction = TMVar SignalType -> TVar [String] -> IO ()

loop :: Config -> LoopFunction -> IO ()
loop conf looper = withDeferSignals $ do
  cls <- mapM (parseTemplate (commands conf) (sepChar conf))
                (splitTemplate (alignSep conf) (template conf))
  let confSig = unSignalChan (signal conf)
  sig <- maybe newEmptyTMVarIO pure confSig
  unless (isJust confSig) $ setupSignalHandler sig
  refLock <- newRefreshLock
  withTimer (refreshLock refLock) $
    bracket (mapM (mapM $ startCommand sig) cls)
            cleanupThreads
            $ \vars -> do
      tv <- initLoop sig refLock vars
      looper sig tv

cleanupThreads :: [[([Async ()], a)]] -> IO ()
cleanupThreads vars =
  for_ (concat vars) $ \(asyncs, _) ->
    for_ asyncs cancel

-- | Initialises context for an event loop, returning a TVar that
-- will hold the current list of values computed by commands.
initLoop :: TMVar SignalType -> TMVar () -> [[([Async ()], TVar String)]]
         -> IO (TVar [String])
initLoop sig lock vs = do
  tv <- newTVarIO ([] :: [String])
  _ <- forkIO (handle (handler "checker") (checker tv [] vs sig lock))
#ifdef DBUS
  runIPC sig
#endif
  return tv
  where
    handler thing (SomeException e) =
      void $ putStrLn ("Thread " ++ thing ++ " failed: " ++ show e)

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

-- | Send signal to eventLoop every time a var is updated
checker :: TVar [String]
           -> [String]
           -> [[([Async ()], TVar String)]]
           -> TMVar SignalType
           -> TMVar ()
           -> IO ()
checker tvar ov vs sig pauser = do
      nval <- atomically $ refreshLockT pauser $ do
              nv <- mapM concatV vs
              guard (nv /= ov)
              writeTVar tvar nv
              return nv
      atomically $ putTMVar sig Wakeup
      checker tvar nval vs sig pauser
    where
      concatV = fmap concat . mapM (readTVar . snd)
