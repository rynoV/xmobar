{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Disk.Freebsd
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

module Xmobar.Plugins.Monitors.Disk.FreeBSD
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

import Xmobar.Plugins.Monitors.Disk.Common (
  DevName
  , Path
  )

import qualified Control.Exception.Extensible as E
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Set as DS
import           Data.Time.Clock.POSIX
import           Foreign
import           Foreign.C.Error (throwErrnoIfMinus1_)
import           Foreign.C.String
import           Foreign.C.Types
import           System.BSD.Sysctl

#include <sys/sysctl.h>
#include <sys/mount.h>
#include <devstat.h>
#include <libgeom.h>

foreign import ccall unsafe "sys/mount.h getfsstat" c_getfsstat :: Ptr STATFS -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "geom_stats_open" c_geom_stats_open :: IO CInt
foreign import ccall unsafe "geom_stats_snapshot_get" c_geom_stats_snapshot_get :: IO (Ptr GSNAP)
foreign import ccall unsafe "&geom_stats_snapshot_free" c_geom_stats_snapshot_free :: FinalizerPtr GSNAP
foreign import ccall unsafe "geom_stats_snapshot_next" c_geom_stats_snapshot_next :: Ptr GSNAP -> IO (Ptr DEVSTAT)
foreign import ccall unsafe "geom_gettree" c_geom_gettree :: Ptr GMESH -> IO CInt
foreign import ccall unsafe "geom_lookupid" c_geom_lookupid :: Ptr GMESH -> Ptr VOIDPTR -> IO (Ptr GIDENT)
foreign import ccall unsafe "&geom_deletetree" c_geom_deletetree :: FinalizerPtr GMESH
foreign import ccall unsafe "geom_stats_snapshot_timestamp" c_geom_stats_snapshot_timestamp :: Ptr GSNAP -> Ptr Timespec -> IO CInt

type DevDataRef = IORef (DM.Map String DevStatData)

data STATFS
data StatFs = StatFs !(ForeignPtr STATFS)
  deriving (Eq, Show)

data DEVSTAT
data DevStat = DevStat !(ForeignPtr DEVSTAT)
  deriving (Eq, Show)

data GMESH
data GMesh = GMesh !(ForeignPtr GMESH)

data GSNAP
data GSnap = GSnap !(ForeignPtr GSNAP)

data GIDENT
data VOIDPTR
data Timespec

data DevStatData = DevStatData {
  devname :: String
  , readDevStat :: Int64
  , writeDevStat :: Int64
  , devstatId :: Ptr VOIDPTR
  , devStatTime :: Rational
  }
  deriving (Show, Eq)

data StatFsData = StatFsData
  {
    fsMntFromName :: String
  , fsMntOnName :: String
  , fsStatBlockSize :: Integer
  -- ^ Optimal transfer block size.
  , fsStatBlockCount :: Integer
  -- ^ Total data blocks in file system.
  , fsStatByteCount :: Integer
  -- ^ Total bytes in file system.
  , fsStatBytesFree :: Integer
  -- ^ Free bytes in file system.
  , fsStatBytesAvailable :: Integer
  -- ^ Free bytes available to non-superusers.
  , fsStatBytesUsed :: Integer
  -- ^ Bytes used.
  }
  deriving (Show, Read, Eq)

data GIdentData = GIdentData
  {
    lgPtr :: Ptr VOIDPTR
  , lgWhat :: CInt
  }
  deriving (Show, Eq)

instance Storable GIdentData where
  alignment _ = #{alignment struct gident}
  sizeOf _    = #{size struct gident}
  peek ptr    = do
    gIdentLgPtr <- #{peek struct gident, lg_ptr} ptr :: IO (Ptr VOIDPTR)
    gIdentLgWhat <- #{peek struct gident, lg_what} ptr :: IO CInt
    return GIdentData {
      lgPtr=gIdentLgPtr
      , lgWhat=gIdentLgWhat
      }

  poke _ _    = pure ()

instance Storable DevStatData where
  alignment _ = #{alignment struct devstat}
  sizeOf _    = #{size struct devstat}
  peek ptr    = do
    device_id <- #{peek struct devstat, id} ptr :: IO (Ptr VOIDPTR)
    device_name <- peekCString $ #{ptr struct devstat, device_name} ptr
    unit_number <- #{peek struct devstat, unit_number} ptr :: IO Int
    bytes_values <- peekArray 4 $ #{ptr struct devstat, bytes} ptr  :: IO [CUIntMax]
    let
      read_value = bytes_values !! #{const DEVSTAT_READ}
      write_value = bytes_values !! #{const DEVSTAT_WRITE}
    return DevStatData {
      devname=concat [device_name, show unit_number]
      , readDevStat=fromInteger . toInteger $ read_value
      , writeDevStat=fromInteger . toInteger $ write_value
      , devstatId=device_id
      , devStatTime=0
      }


  poke _ _    = pure ()

instance Storable StatFsData where
  alignment _ = #{alignment struct statfs}
  sizeOf _    = #{size struct statfs}
  peek ptr    = do
       fmntfromname <- peekCString $ #{ptr struct statfs, f_mntfromname} ptr
       fmntonname <- peekCString $ #{ptr struct statfs, f_mntonname} ptr
       bsize <- #{peek struct statfs, f_bsize} ptr
       bcount <- #{peek struct statfs, f_blocks} ptr
       bfree <- #{peek struct statfs, f_bfree} ptr
       bavail <- #{peek struct statfs, f_bavail} ptr
       let
         bpb = toI bsize
       return $ StatFsData {
         fsMntFromName = fmntfromname
         , fsMntOnName = fmntonname
         , fsStatBlockSize = bpb
         , fsStatBlockCount = toI bcount
         , fsStatByteCount = toI bcount * bpb
         , fsStatBytesFree = toI bfree * bpb
         , fsStatBytesAvailable = toI bavail * bpb
         , fsStatBytesUsed = toI (bcount - bfree) * bpb
         }

  poke _ _    = pure ()


toI :: CULong -> Integer
toI = toInteger

mountCount :: IO CInt
mountCount = c_getfsstat nullPtr 0 #{const MNT_NOWAIT}

getMountInfo :: IO [StatFsData]
getMountInfo = do
  cmountcount <- mountCount
  let
    cbytes = cmountcount * #{size struct statfs}
    bytes = fromInteger . toInteger $ cbytes
    mountcount = fromInteger . toInteger $ cmountcount
  allocaBytes bytes $ \vfs -> do
    c_getfsstat vfs cbytes #{const MNT_NOWAIT}
    peekArray mountcount $ castPtr vfs :: IO [StatFsData]

cTimeToInteger :: CTime -> Integer
cTimeToInteger (CTime n) = fromIntegral n

getSnapshotTime :: GSnap -> IO Integer
getSnapshotTime (GSnap snap_fp) = do
  allocaBytes #{const sizeof(struct timespec)} $ \p_ts -> do
    withForeignPtr snap_fp $ \snap_ptr -> do
      throwErrnoIfMinus1_ "geom_stats_snapshot_timestamp"
        $ c_geom_stats_snapshot_timestamp snap_ptr p_ts
      u_sec  <- #{peek struct timespec,tv_sec}  p_ts :: IO CTime
      u_nsec <- #{peek struct timespec,tv_nsec} p_ts :: IO CLong
      return (cTimeToInteger u_sec * 1e12 + fromIntegral u_nsec * 1e3)

checkGeomStat' :: GIdentData -> GSnap -> DevStatData -> [DevStatData] -> IO [DevStatData]
checkGeomStat' gident_data gsnap stat acc
  | (lgWhat gident_data) /= #{const ISPROVIDER} = return acc
  | otherwise = do
      lgNamePtr <- #{peek struct gprovider, lg_name} $ lgPtr gident_data
      lgName <- peekCString $ castPtr lgNamePtr
      lgTime <- toRational <$> getSnapshotTime gsnap
      return $ stat
        {
          devname=concat ["/dev/", lgName]
        , devStatTime= lgTime / 1e12
        } : acc


checkGeomStat :: Ptr GIDENT -> GSnap -> DevStatData -> [DevStatData] -> IO [DevStatData]
checkGeomStat gident_ptr gsnap stat acc
  | gident_ptr == nullPtr = return acc
  | otherwise = do
      gIdent <- peek $ castPtr gident_ptr :: IO GIdentData
      checkGeomStat' gIdent gsnap stat acc


getGeomStats' :: GMesh -> GSnap -> Ptr DEVSTAT -> [DevStatData] -> IO [DevStatData]
getGeomStats' gmeshD@(GMesh gmesh_fp) gsnapD@(GSnap snap_fp) ptr acc
  | ptr == nullPtr = return acc
  | otherwise = do
      withForeignPtr snap_fp $ \snap_ptr -> do
        acc' <- withForeignPtr gmesh_fp $ \gmesh_ptr -> do
          stat <- (peek $ castPtr ptr) :: IO DevStatData
          gIdentPtr <- c_geom_lookupid gmesh_ptr (devstatId stat)
          checkGeomStat gIdentPtr gsnapD stat acc
        nextStatPtr <- c_geom_stats_snapshot_next snap_ptr
        getGeomStats' gmeshD gsnapD nextStatPtr acc'

getGeomStats :: IO [DevStatData]
getGeomStats = do
  gmesh_fp <- mallocForeignPtrBytes bytesmesh
  addForeignPtrFinalizer c_geom_deletetree gmesh_fp
  c_geom_stats_open
  withForeignPtr gmesh_fp $ \gmesh_ptr -> do
    c_geom_gettree gmesh_ptr
    snap_ptr <- c_geom_stats_snapshot_get
    snap_fp <- newForeignPtr c_geom_stats_snapshot_free snap_ptr
    withForeignPtr snap_fp $ \snap_ptr' -> do
      nextStatPtr <- c_geom_stats_snapshot_next snap_ptr'
      getGeomStats' (GMesh gmesh_fp) (GSnap snap_fp) nextStatPtr []
  where
    bytesmesh = #{size struct gmesh}


readGeomStats :: DM.Map String DevStatData -> IO (DM.Map String DevStatData)
readGeomStats acc = do
  (Prelude.foldr (\x-> DM.insert (devname x) x) acc) <$> getGeomStats

defaultDevStatData :: DevStatData
defaultDevStatData = DevStatData
  {
    devname = ""
  , readDevStat = 0
  , writeDevStat = 0
  , devstatId = nullPtr
  , devStatTime = 0
  }

sysctlNextOid :: [Int32] -> IO [Int32]
sysctlNextOid oid = do
  let query_oid = #{const CTL_SYSCTL} : #{const CTL_SYSCTL_NEXT} : oid
  E.catch (sysctlPeekArray query_oid :: IO [Int32]) (\(E.SomeException _) -> return [])

sysctlOidToName :: [Int32] -> IO String
sysctlOidToName oid = do
  let query_oid = #{const CTL_SYSCTL} : #{const CTL_SYSCTL_NAME} : oid
  nameO <- sysctlReadString query_oid
  return nameO

fetchZfsStat :: [Int32] -> DM.Map (String, String) DevStatData -> [String] -> IO (DM.Map (String, String) DevStatData)
fetchZfsStat oid acc (_ : _ : poolName : "dataset" : refName : "nread" : []) = do
  readsB <- sysctlReadLong oid
  let val = DM.findWithDefault defaultDevStatData (poolName, refName) acc
      val' = val
        {
          readDevStat = readsB
        }
  return $ DM.insert (poolName, refName) val' acc

fetchZfsStat oid acc (_ : _ : poolName : "dataset" : refName : "nwritten" : []) = do
  writesB <- sysctlReadLong oid
  let val = DM.findWithDefault defaultDevStatData (poolName, refName) acc
      val' = val
        {
          writeDevStat = writesB
        }
  return $ DM.insert (poolName, refName) val' acc

fetchZfsStat oid acc (_ : _ : poolName : "dataset" : refName : "dataset_name" : []) = do
  datasetName <- sysctlReadString oid
  datasetTime <- toRational <$> getPOSIXTime
  let val = DM.findWithDefault defaultDevStatData (poolName, refName) acc
      val' = val
        {
          devname = datasetName
        , devStatTime = datasetTime
        }
  return $ DM.insert (poolName, refName) val' acc

fetchZfsStat _ acc _ = return acc

readZfsStat' :: [Int32] -> [Int32] -> DM.Map (String, String) DevStatData -> IO (DM.Map (String, String) DevStatData)
readZfsStat' mainOid actOid acc
   | mainOid `DL.isPrefixOf` actOid = do
       nameDS <- sysctlOidToName actOid
       let nameArr = splitOnDot nameDS
       acc' <- fetchZfsStat actOid acc nameArr
       nextOid <- sysctlNextOid actOid
       readZfsStat' mainOid nextOid acc'

   | otherwise = return acc

splitOnDot :: String -> [String]
splitOnDot [] = [[]]
splitOnDot ('.':xs) = [] : splitOnDot xs
splitOnDot (x:xs) =
           let rest = splitOnDot xs
           in (x : head rest) : tail rest

readZfsStats :: DM.Map DevName DevStatData -> IO (DM.Map DevName DevStatData)
readZfsStats acc = do
  mainO <- sysctlNameToOid "kstat.zfs"
  mainOid <- sysctlExtractOid mainO
  (DM.foldr (\x-> DM.insert (devname x) x) acc) <$> (readZfsStat' mainOid mainOid $ DM.empty)

readDevsStats :: IO (DM.Map DevName DevStatData)
readDevsStats = do
  geomStats <- readGeomStats DM.empty
  readZfsStats geomStats

extractDataIO :: DM.Map String DevStatData -> DM.Map String DevStatData -> String -> (DevName, [Float])
extractDataIO currs prevs disk = (disk, diffStat)
  where
    diffStat = [sp, rSp, wSp, fromInteger t, fromInteger  r, fromInteger w]
    r = toInteger $ (readDevStat curr) - (readDevStat prev)
    w = toInteger $ (writeDevStat curr) - (writeDevStat prev)
    t = r + w
    rSp = speed r diffTime
    wSp = speed w diffTime
    sp =  speed t diffTime
    curr = DM.findWithDefault defaultDevStatData disk currs
    prev = DM.findWithDefault defaultDevStatData disk prevs
    diffTime = (devStatTime curr) - (devStatTime prev)
    speed :: Integer -> Rational -> Float
    speed _ 0 = 0
    speed x d = (fromInteger x) / (realToFrac d)

fetchDataIO :: DevDataRef -> [(String, String)] -> IO [(DevName, [Float])]
fetchDataIO dref disks = do
  currStats <- readDevsStats
  prevStats <- readIORef dref
  writeIORef dref currStats
  return $ map (extractDataIO currStats prevStats) $ mountedOrDiskDevices disks currStats

fetchDataUsage :: [(String, String)] -> IO [((DevName, Path), [Integer])]
fetchDataUsage disks = Prelude.map extractStat <$> Prelude.filter isReq <$> getMountInfo
  where
    req = Prelude.map fst disks
    isReq :: StatFsData -> Bool
    isReq stat = (fsMntOnName stat) `elem` req
                 || Prelude.drop 5 (fsMntFromName stat) `elem` req
                 || (fsMntFromName stat) `elem` req
    extractStat :: StatFsData -> ((String, String), [Integer])
    extractStat stat = ((fsMntFromName stat, fsMntOnName stat)
      , [
          fsStatByteCount stat
        , fsStatBytesFree stat
        , fsStatBytesUsed stat
        ]
      )

initializeDevDataRef :: [(String, String)] -> IO DevDataRef
initializeDevDataRef _ = do
  stats <- readDevsStats
  newIORef stats

mountedOrDiskDevices :: [(DevName, Path)] -> DM.Map String DevStatData -> [DevName]
mountedOrDiskDevices mounted devs = DS.elems $ mountedOrDiskDevices' mountedAcc (DM.keys devs)
  where
    mountedAcc = mountedOrDiskDevices' DS.empty (map fst mounted)

mountedOrDiskDevices' :: DS.Set DevName -> [DevName] -> DS.Set DevName
mountedOrDiskDevices' acc [] = acc
mountedOrDiskDevices' acc (x:xs) = mountedOrDiskDevices' (DS.insert x acc) xs
