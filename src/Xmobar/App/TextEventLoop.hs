{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.TextEventLoop
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Fri Jan 28, 2022 01:21
--
--
-- Text-only event loop
--
------------------------------------------------------------------------------

module Xmobar.App.TextEventLoop (startTextLoop) where

import Prelude hiding (lookup)

import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Exception (handle, SomeException(..))

import Xmobar.System.Signal
import Xmobar.Config.Types (Config)

import Xmobar.X11.Parsers (parseStringAsText)

import Xmobar.App.CommandThreads (refreshLockT)

#ifdef DBUS
import Xmobar.System.DBus
#endif

-- | Starts the main event loop and threads
startTextLoop :: Config
              -> TMVar SignalType
              -> TMVar ()
              -> [[([Async ()], TVar String)]]
              -> IO ()
startTextLoop cfg sig pauser vs = do
    tv <- newTVarIO []
    _ <- forkIO (handle (handler "checker") (checker tv [] vs sig pauser))
#ifdef DBUS
    runIPC sig
#endif
    eventLoop cfg tv sig
  where
    handler thing (SomeException e) =
      void $ putStrLn ("Thread " ++ thing ++ " failed: " ++ show e)

-- | Send signal to eventLoop every time a var is updated
checker :: TVar [String]
           -> [String]
           -> [[([Async ()], TVar String)]]
           -> TMVar SignalType
           -> TMVar ()
           -> IO ()
checker tvar ov vs signal pauser = do
      nval <- atomically $ refreshLockT pauser $ do
              nv <- mapM concatV vs
              guard (nv /= ov)
              writeTVar tvar nv
              return nv
      atomically $ putTMVar signal Wakeup
      checker tvar nval vs signal pauser
    where
      concatV = fmap concat . mapM (readTVar . snd)

-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: Config -> TVar [String] -> TMVar SignalType -> IO ()
eventLoop cfg tv signal = do
  typ <- atomically $ takeTMVar signal
  case typ of
    Wakeup -> updateString cfg tv >>= putStrLn >> eventLoop cfg tv signal
    _ -> eventLoop cfg tv signal

updateString :: Config -> TVar [String] -> IO String
updateString conf v = do
  s <- readTVarIO v
  let l:c:r:_ = s ++ repeat ""
  liftIO $ concat `fmap` mapM (parseStringAsText conf) [l, c, r]
