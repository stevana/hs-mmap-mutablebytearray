{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Control.Exception
import Control.Monad
import Foreign hiding (addForeignPtrFinalizer)
import Foreign.Concurrent
import GHC.Exts
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(MallocPtr),
                       mallocForeignPtrAlignedBytes)
import GHC.Types
import System.Directory
import System.Environment
import System.Posix.Fcntl
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

import Mmap

------------------------------------------------------------------------

data MBA = MBA
  { unMBA :: MutableByteArray# RealWorld }

mbaPtr :: MBA -> Ptr a
mbaPtr (MBA mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))

allocateAligned :: Int -> Int -> IO MBA
allocateAligned capa@(I# capa#) align@(I# align#) = IO $ \s ->
  case newAlignedPinnedByteArray# capa# align# s of
    (# s', mba# #) -> (# s', MBA mba# #)

mmapped :: FilePath -> Int -> IO MBA
mmapped fp capa = do
  fd <- openFd fp ReadWrite Nothing defaultFileFlags
  pageSize <- sysconfPageSize
  mba <- allocateAligned capa pageSize
  ptr <- mmap (Just (mbaPtr mba)) (fromIntegral capa)
            (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0
  if ptr /= mbaPtr mba
  then error "not same pointer"
  else return mba

mmappedAnon :: Int -> IO MBA
mmappedAnon capa = do
  pageSize <- sysconfPageSize
  mba <- allocateAligned capa pageSize
  ptr <- mmap (Just (mbaPtr mba)) (fromIntegral capa)
            (pROT_READ .|. pROT_WRITE) mAP_SHARED Nothing 0
  if ptr /= mbaPtr mba
  then error "not same pointer"
  else return mba

freeMBA :: MBA -> IO ()
freeMBA mba = do
  size <- IO $ \s -> case sizeofMutableByteArray# (unMBA mba) of
                       size# -> (# s, I# size# #)
  munmap (mbaPtr mba) (fromIntegral size)

------------------------------------------------------------------------

works :: IO ()
works = replicateM_ 200000 $ do
  pageSize <- sysconfPageSize
  _mba <- mmappedAnon pageSize
  return ()

fallocate :: FilePath -> IO ()
fallocate fp = do
  b <- fileExist fp
  when (not b) $ do
    fd <- openFd fp ReadWrite (Just (ownerReadMode .|. ownerWriteMode)) defaultFileFlags
    pageSize <- sysconfPageSize
    fileAllocate fd 0 (fromIntegral pageSize)
    closeFd fd

-- hsc2hs Mmap.hsc && ghc -debug -g --make ./Main.hs && ./Main +RTS -DS
_main :: IO ()
_main = do
  let fp = "/tmp/mmap-bug.txt"
  fallocate fp
  replicateM_ 200000 $ do
    pageSize <- sysconfPageSize
    mba <- mmapped fp pageSize
    -- freeMBA mba -- Commenting this out gives "Main: internal error: scavenge_one:
                -- strange object 1095218 (GHC version 8.10.7 for
                -- x86_64_unknown_linux)" and leaving it in causes a segfault.
    -- With GHC version 9.0.1 the internal error is fixed and both segfault.
    return ()

------------------------------------------------------------------------

data MBA' = MBA' { unMBA' :: ForeignPtr Word8 }

allocateAligned' :: Int -> Int -> IO MBA'
allocateAligned' size align = do
  fptr <- mallocForeignPtrAlignedBytes size align
  return (MBA' fptr)

mbaPtr' :: MBA' -> Ptr Word8
mbaPtr' (MBA' (ForeignPtr addr# (MallocPtr _mba# _finaliser))) = Ptr addr#

mmapped' :: FilePath -> Int -> IO MBA'
mmapped' fp size = do
  fd <- openFd fp ReadWrite Nothing defaultFileFlags
  pageSize <- sysconfPageSize
  mba@(MBA' fptr@(ForeignPtr addr# _)) <- allocateAligned' size pageSize
  ptr <- mmap (Just (mbaPtr' mba)) (fromIntegral pageSize)
            (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0

  addForeignPtrFinalizer fptr $ do
    munmap ptr (fromIntegral pageSize)
  -- The following `closeFd` is ok, because:
  -- "The mmap() function adds an extra reference to the file associated with the
  -- file descriptor fildes which is not removed by a subsequent close() on that
  -- file descriptor. This reference is removed when there are no more mappings
  -- to the file." -- https://pubs.opengroup.org/onlinepubs/7908799/xsh/mmap.html
  closeFd fd
  if ptr /= mbaPtr' mba
  then error "not same pointer"
  else return mba

freeMBA' :: MBA' -> IO ()
freeMBA' (MBA' fptr) = do
--   munmap (Ptr addr#) (fromIntegral pageSize)
  finalizeForeignPtr fptr

-- https://stackoverflow.com/questions/51822946/how-to-align-in-memory-the-array-payload-of-a-bytearray-with-ghc-haskell

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      let fp = "/tmp/mmap-bug.txt"
      fallocate fp
      replicateM_ 20000 $ do
        pageSize <- sysconfPageSize
        mba <- mmapped' fp 64
        -- This line seems to cause the segfault:
        -- https://gitlab.haskell.org/ghc/ghc/-/blob/master/rts/sm/BlockAlloc.c#L833
        freeMBA' mba
        return ()
    ["works"] -> worksMmap
