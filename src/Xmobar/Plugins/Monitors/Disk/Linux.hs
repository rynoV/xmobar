-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Disk.Linux
-- Copyright   :  (c) 2010, 2011, 2012, 2014, 2018, 2019 Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Disk usage and throughput monitors for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Disk.Linux
  (
    fetchDataIO
  , fetchDataUsage
  , initializeDevDataRef
  , DevDataRef
  ) where

import Data.IORef (
  IORef
  , newIORef
  , readIORef
  , writeIORef
  )

import Xmobar.System.StatFS (
  getFileSystemStats
  , fsStatByteCount
  , fsStatBytesAvailable
  , fsStatBytesUsed
  )
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, find)
import Data.Maybe (catMaybes)
import System.Directory (canonicalizePath, doesFileExist)
import Control.Exception (SomeException, handle)

import Xmobar.Plugins.Monitors.Disk.Common (
  DevName
  , Path
  )

type DevDataRef = IORef [(DevName, [Float])]

fsStats :: String -> IO [Integer]
fsStats path = do
  stats <- getFileSystemStats path
  case stats of
    Nothing -> return [0, 0, 0]
    Just f -> let tot = fsStatByteCount f
                  free = fsStatBytesAvailable f
                  used = fsStatBytesUsed f
              in return [tot, free, used]

mountedDevices :: [String] -> IO [(DevName, Path)]
mountedDevices req = do
  s <- B.readFile "/etc/mtab"
  parse `fmap` mapM mbcanon (devs s)
  where
    mbcanon (d, p) = doesFileExist d >>= \e ->
                     if e
                        then Just `fmap` canon (d,p)
                        else return Nothing
    canon (d, p) = do {d' <- canonicalizePath d; return (d', p)}
    devs = filter isDev . map (firstTwo . B.words) . B.lines
    parse = map undev . filter isReq . catMaybes
    firstTwo (a:b:_) = (B.unpack a, B.unpack b)
    firstTwo _ = ("", "")
    isDev (d, _) = "/dev/" `isPrefixOf` d
    isReq (d, p) = p `elem` req || drop 5 d `elem` req
    undev (d, f) = (drop 5 d, f)

diskDevices :: [String] -> IO [(DevName, Path)]
diskDevices req = do
  s <- B.readFile "/proc/diskstats"
  parse `fmap` mapM canon (devs s)
  where
    canon (d, p) = do {d' <- canonicalizePath d; return (d', p)}
    devs = map (third . B.words) . B.lines
    parse = map undev . filter isReq
    third (_:_:c:_) = ("/dev/" ++ B.unpack c, B.unpack c)
    third _ = ("", "")
    isReq (d, p) = p `elem` req || drop 5 d `elem` req
    undev (d, f) = (drop 5 d, f)

mountedOrDiskDevices :: [String] -> IO [(DevName, Path)]
mountedOrDiskDevices req = do
  mnt <- mountedDevices req
  case mnt of
       []    -> diskDevices req
       other -> return other

diskData :: IO [(DevName, [Float])]
diskData = do
  s <- B.readFile "/proc/diskstats"
  let extract ws = (head ws, map read (tail ws))
  return $ map (extract . map B.unpack . drop 2 . B.words) (B.lines s)

mountedData :: DevDataRef -> [DevName] -> IO [(DevName, [Float])]
mountedData dref devs = do
  dt <- readIORef dref
  dt' <- diskData
  writeIORef dref dt'
  return $ map (parseDev (zipWith diff dt' dt)) devs
  where diff (dev, xs) (_, ys) = (dev, zipWith (-) xs ys)


parseDev :: [(DevName, [Float])] -> DevName -> (DevName, [Float])
parseDev dat dev =
  case find ((==dev) . fst) dat of
    Nothing -> (dev, [0, 0, 0])
    Just (_, xs) ->
      let r = 4096 * xs !! 2
          w = 4096 * xs !! 6
          t = r + w
          rSp = speed r (xs !! 3)
          wSp = speed w (xs !! 7)
          sp =  speed t (xs !! 3 + xs !! 7)
          speed x d = if d == 0 then 0 else x / d
          dat' = if length xs > 6
                 then [sp, rSp, wSp, t, r, w]
                 else [0, 0, 0, 0, 0, 0]
      in (dev, dat')

fetchDataIO :: DevDataRef -> [(String, String)] -> IO [(String, [Float])]
fetchDataIO dref disks = do
  dev <- mountedOrDiskDevices (map fst disks)
  mountedData dref (map fst dev)

fetchDataUsage :: [(String, String)] -> IO [((String, String), [Integer])]
fetchDataUsage disks = do
  devs <- mountedDevices (map fst disks)
  mapM fetchStats devs
  where
    fetchStats :: (String, String) -> IO ((String, String), [Integer])
    fetchStats (dev, path) = do
      stats <- handle ign $ fsStats path
      return ((dev, path), stats)
    ign = const (return [0, 0, 0]) :: SomeException -> IO [Integer]

initializeDevDataRef :: [(String, String)] -> IO DevDataRef
initializeDevDataRef disks = do
  dev <- mountedOrDiskDevices (map fst disks)
  newIORef (map (\d -> (fst d, repeat 0)) dev)
