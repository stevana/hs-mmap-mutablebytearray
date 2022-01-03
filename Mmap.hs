{-# LINE 1 "Mmap.hsc" #-}
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


_SC_PAGE_SIZE = 30
{-# LINE 28 "Mmap.hsc" #-}


mAP_ANONYMOUS = 32
{-# LINE 31 "Mmap.hsc" #-}
mAP_FIXED     = 16
{-# LINE 32 "Mmap.hsc" #-}

mAP_SHARED    = 1
{-# LINE 34 "Mmap.hsc" #-}
mAP_PRIVATE   = 2
{-# LINE 35 "Mmap.hsc" #-}

pROT_READ     = 1
{-# LINE 37 "Mmap.hsc" #-}
pROT_WRITE    = 2
{-# LINE 38 "Mmap.hsc" #-}
pROT_EXEC     = 4
{-# LINE 39 "Mmap.hsc" #-}
pROT_NONE     = 0
{-# LINE 40 "Mmap.hsc" #-}

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
