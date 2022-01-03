module Mmap where

import System.Posix.Types
import Foreign.C.Types
import Data.Maybe (fromMaybe)
import Foreign.C.Error
import Foreign
import System.Posix.IO
import Control.Monad
import GHC.Exts

------------------------------------------------------------------------

foreign import ccall unsafe "sys/mman.h mmap"
  c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  c_munmap :: Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "stdlib.h posix_memalign"
  c_posix_memalign :: Ptr (Ptr a) -> CSize -> CSize -> IO CInt

foreign import ccall unsafe "unistd.h sysconf"
  c_sysconf :: CInt -> IO CLong

#include <unistd.h>
_SC_PAGE_SIZE = #const _SC_PAGE_SIZE

#include <sys/mman.h>
mAP_ANONYMOUS = #const MAP_ANONYMOUS
mAP_FIXED     = #const MAP_FIXED

mAP_SHARED    = #const MAP_SHARED
mAP_PRIVATE   = #const MAP_PRIVATE

pROT_READ     = #const PROT_READ
pROT_WRITE    = #const PROT_WRITE
pROT_EXEC     = #const PROT_EXEC
pROT_NONE     = #const PROT_NONE

------------------------------------------------------------------------

posixMemalign :: Int -> Int -> IO (Ptr (Ptr a), Ptr a)
posixMemalign align size = do
  memPtr <- malloc
  throwErrnoIfMinus1_ "posix_memalign"
    (c_posix_memalign memPtr (fromIntegral align) (fromIntegral size))
  ptr <- peek memPtr
  return (memPtr, ptr)

sysconfPageSize :: IO Int
sysconfPageSize = fromIntegral <$> c_sysconf _SC_PAGE_SIZE

------------------------------------------------------------------------

mmap :: Maybe (Ptr a) -> Int -> CInt -> CInt -> Maybe Fd
     -> COff -> IO (Ptr a)
mmap mAddr len prot visib mFd offset =
  throwErrnoIf (== mAP_FAILED) "mmap" $
    c_mmap addr (fromIntegral len) prot flags fd offset
  where
    mAP_FAILED = nullPtr `plusPtr` (-1)

    addr = fromMaybe nullPtr mAddr

    fd :: CInt
    fd = case mFd of
      Nothing        -> (-1)
      Just (Fd cint) -> cint

    flags :: CInt
    flags =  maybe mAP_ANONYMOUS (const 0) mFd
         .|. maybe 0 (const mAP_FIXED) mAddr
         .|. visib

munmap :: Ptr a -> CSize -> IO ()
munmap addr len = throwErrnoIfMinus1_ "munmap" (c_munmap addr len)

------------------------------------------------------------------------

worksMmap :: IO ()
worksMmap = do
  putStrLn "Starting test"
  pageSz <- sysconfPageSize
  let sz = pageSz
  flip mapM_ [0..2000000] $ \i -> do
    when (i `mod` 100000 == 0) $
      putStrLn $ "iteration: " ++ show i
    (memPtr, ptr') <- posixMemalign pageSz pageSz
    fd <- openFd "/tmp/mmap.txt" ReadWrite Nothing defaultFileFlags
    ptr <- mmap (Just ptr') sz (pROT_READ .|. pROT_WRITE) mAP_SHARED (Just fd) 0
    if ptr /= ptr'
    then error "not same ptr"
    else do
      munmap ptr (fromIntegral sz) -- This isn't needed.
      free memPtr   -- Commenting this free out causes a seg fault.
      free ptr'  -- Commenting this free out causes "mmap: resource exhausted
                 -- (Cannot allocate memory)".

      closeFd fd -- Commenting this out causes "openFd: resource exhausted (Too
                 -- many open files)".
