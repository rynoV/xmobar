{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.X11EventLoop
-- Copyright: (c) 2018, 2020, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 19:40
--
--
-- Event loop
--
------------------------------------------------------------------------------

module Xmobar.App.X11EventLoop (x11Loop) where

import Prelude hiding (lookup)
import Graphics.X11.Xlib hiding (textExtents, textWidth, Segment)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Control.Exception (handle, SomeException(..))
import Data.Bits
import Data.Map hiding (foldr, map, filter)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))

import Data.Maybe (fromJust, isJust)
import qualified Data.List.NonEmpty as NE

import Xmobar.System.Signal
import Xmobar.Config.Types ( persistent
                           , font
                           , additionalFonts
                           , textOffset
                           , textOffsets
                           , position
                           , iconRoot
                           , Config
                           , Align(..)
                           , XPosition(..))

import Xmobar.X11.Actions
import Xmobar.X11.Parsers
import Xmobar.X11.Window
import Xmobar.X11.Text
import Xmobar.X11.Draw
import Xmobar.X11.Bitmap as Bitmap
import Xmobar.X11.Types
import Xmobar.System.Utils (safeIndex)

import Xmobar.App.CommandThreads (initLoop, loop)

#ifndef THREADED_RUNTIME
import Xmobar.X11.Events(nextEvent')
#endif

#ifdef XFT
import Graphics.X11.Xft
#endif

runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc

-- | Starts the main event loop and threads
x11Loop :: Config -> IO ()
x11Loop conf = do
  initThreads
  d <- openDisplay ""
  fs <- initFont d (font conf)
  fl <- mapM (initFont d) (additionalFonts conf)
  let ic = Map.empty
      to = textOffset conf
      ts = textOffsets conf ++ replicate (length fl) (-1)
  loop conf $ \sig lock vars -> do
    (r,w) <- createWin d fs conf
    startLoop (XConf d r w (fs :| fl) (to :| ts) ic conf) sig lock vars

startLoop :: XConf
          -> TMVar SignalType
          -> TMVar ()
          -> [[([Async ()], TVar String)]]
          -> IO ()
startLoop xcfg@(XConf _ _ w _ _ _ _) sig pauser vs = do
#ifdef XFT
    xftInitFtLibrary
#endif
    tv <- initLoop sig pauser vs
#ifdef THREADED_RUNTIME
    _ <- forkOS (handle (handler "eventer") (eventer sig))
#else
    _ <- forkIO (handle (handler "eventer") (eventer sig))
#endif
    eventLoop tv xcfg [] sig
  where
    handler thing (SomeException e) =
      void $ putStrLn ("Thread " ++ thing ++ " failed: " ++ show e)
    -- Reacts on events from X
    eventer signal =
      allocaXEvent $ \e -> do
        dpy <- openDisplay ""
        xrrSelectInput dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
        selectInput dpy w (exposureMask .|. structureNotifyMask .|. buttonPressMask)

        forever $ do
#ifdef THREADED_RUNTIME
          nextEvent dpy e
#else
          nextEvent' dpy e
#endif
          ev <- getEvent e
          case ev of
            ConfigureEvent {} -> atomically $ putTMVar signal Reposition
            ExposeEvent {} -> atomically $ putTMVar signal Wakeup
            RRScreenChangeNotifyEvent {} -> atomically $ putTMVar signal Reposition
            ButtonEvent {} -> atomically $
                   putTMVar signal (Action (ev_button ev) (fi $ ev_x ev))
            _ -> return ()

-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: TVar [String]
             -> XConf
             -> [([Action], Position, Position)]
             -> TMVar SignalType
             -> IO ()
eventLoop tv xc@(XConf d r w fs vos is cfg) as signal = do
      typ <- atomically $ takeTMVar signal
      case typ of
         Wakeup -> do
            str <- updateString cfg tv
            xc' <- updateCache d w is (iconRoot cfg) str >>=
                     \c -> return xc { iconS = c }
            as' <- updateActions xc r str
            runX xc' $ drawInWin r str
            eventLoop tv xc' as' signal

         Reposition ->
            reposWindow cfg

         ChangeScreen -> do
            ncfg <- updateConfigPosition cfg
            reposWindow ncfg

         Hide   t -> hide   (t*100*1000)
         Reveal t -> reveal (t*100*1000)
         Toggle t -> toggle t

         TogglePersistent -> eventLoop
            tv xc { config = cfg { persistent = not $ persistent cfg } } as signal

         Action but x -> action but x

    where
        isPersistent = not $ persistent cfg

        hide t
            | t == 0 =
                when isPersistent (hideWindow d w) >> eventLoop tv xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Hide 0)
                eventLoop tv xc as signal

        reveal t
            | t == 0 = do
                when isPersistent (showWindow r cfg d w)
                eventLoop tv xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Reveal 0)
                eventLoop tv xc as signal

        toggle t = do
            ismapped <- isMapped d w
            atomically (putTMVar signal $ if ismapped then Hide t else Reveal t)
            eventLoop tv xc as signal

        reposWindow rcfg = do
          r' <- repositionWin d w (NE.head fs) rcfg
          eventLoop tv (XConf d r' w fs vos is rcfg) as signal

        updateConfigPosition ocfg =
          case position ocfg of
            OnScreen n o -> do
              srs <- getScreenInfo d
              return (if n == length srs
                       then
                        (ocfg {position = OnScreen 1 o})
                       else
                        (ocfg {position = OnScreen (n+1) o}))
            o -> return (ocfg {position = OnScreen 1 o})

        action button x = do
          mapM_ runAction $
            filter (\(Spawn b _) -> button `elem` b) $
            concatMap (\(a,_,_) -> a) $
            filter (\(_, from, to) -> x >= from && x <= to) as
          eventLoop tv xc as signal

updateString :: Config -> TVar [String] -> IO [[Segment]]
updateString conf v = do
  s <- readTVarIO v
  let l:c:r:_ = s ++ repeat ""
  liftIO $ mapM (parseString conf) [l, c, r]

updateActions :: XConf -> Rectangle -> [[Segment]]
              -> IO [([Action], Position, Position)]
updateActions conf (Rectangle _ _ wid _) ~[left,center,right] = do
  let (d,fs) = (display &&& fontListS) conf
      strLn :: [Segment] -> IO [(Maybe [Action], Position, Position)]
      strLn  = liftIO . mapM getCoords
      iconW i = maybe 0 Bitmap.width (lookup i $ iconS conf)
      getCoords (Text s,_,i,a) =
        textWidth d (safeIndex fs i) s >>= \tw -> return (a, 0, fi tw)
      getCoords (Icon s,_,_,a) = return (a, 0, fi $ iconW s)
      getCoords (Hspace w,_,_,a) = return (a, 0, fi w)
      partCoord off xs = map (\(a, x, x') -> (fromJust a, x, x')) $
                         filter (\(a, _,_) -> isJust a) $
                         scanl (\(_,_,x') (a,_,w') -> (a, x', x' + w'))
                               (Nothing, 0, off)
                               xs
      totSLen = foldr (\(_,_,len) -> (+) len) 0
      remWidth xs = fi wid - totSLen xs
      offs = 1
      offset a xs = case a of
                     C -> (remWidth xs + offs) `div` 2
                     R -> remWidth xs
                     L -> offs
  fmap concat $ mapM (\(a,xs) ->
                       (\xs' -> partCoord (offset a xs') xs') <$> strLn xs) $
                     zip [L,C,R] [left,center,right]
