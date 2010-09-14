{-# LANGUAGE ForeignFunctionInterface #-}

{-
 - Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions
 - are met:
 - 1. Redistributions of source code must retain the above copyright
 -    notice, this list of conditions and the following disclaimer.
 - 2. Redistributions in binary form must reproduce the above copyright
 -    notice, this list of conditions and the following disclaimer in the
 -    documentation and/or other materials provided with the distribution.
 - 3. The name of the author may not be used to endorse or promote products
 -    derived from this software without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 - IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 - OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 - IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 - INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 - NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 - DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 - THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 - (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 - THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

--{{{ Exports
{-|
    Module:      System.PinkTrace.SysCall
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: non-portable, requires freebsd or linux

    Pink's system call utility functions
-}
module System.PinkTrace.SystemCall
    ( SystemCallReturn
    , nameSystemCall
    , lookupSystemCall
    , getSystemCallNumber
    , setSystemCallNumber
    , getSystemCallReturn
    , setSystemCallReturn
    , getSystemCallArgument
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Control.Monad         (when)
import Foreign.C.Error       (throwErrno)
import Foreign.C.String      (CString, peekCString, withCString)
import Foreign.C.Types       (CInt, CLong, CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr           (Ptr, nullPtr)
import Foreign.Storable      (peek)
import System.Posix.Types    (CPid, ProcessID)

import System.PinkTrace         (Index(..))
import System.PinkTrace.Bitness ( Bitness(..)
                                , bitness32Supported
                                , bitness64Supported
                                )
--}}}
--{{{ Foreign Imports
foreign import ccall pink_name_syscall :: CLong -> CInt -> CString
foreign import ccall pink_name_lookup :: CString -> CInt -> CLong
foreign import ccall pink_util_get_syscall :: CPid -> CInt -> Ptr CLong -> IO CInt
foreign import ccall pink_util_set_syscall :: CPid -> CInt -> CLong -> IO CInt
foreign import ccall pink_util_get_return :: CPid -> Ptr CLong -> IO CInt
foreign import ccall pink_util_set_return :: CPid -> CLong -> IO CInt
foreign import ccall pink_util_get_arg :: CPid -> CInt -> CUInt -> Ptr CLong -> IO CInt
--}}}
--{{{ Types
type SystemCallReturn = CLong
--}}}
--{{{ Functions
{-|
    Return the name of the given system call.
-}
nameSystemCall :: Bitness           -- ^ The bitness of the traced child
               -> Int               -- ^ System call number
               -> IO (Maybe String) -- ^ Just @name@ if system call is valid, 'Nothing' otherwise
nameSystemCall bit scno
    | bit == Bitness32 && not bitness32Supported = error $ "nameSystemCall: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "nameSystemCall: unsupported bitness " ++ show bit
    | otherwise = if scname == nullPtr then return Nothing else fmap Just (peekCString scname)
    where
        scno' :: CLong
        scno' = fromIntegral scno
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit
        scname :: CString
        scname = pink_name_syscall scno' bit'

{-|
    Look up the number of the given system call name.
-}
lookupSystemCall :: Bitness        -- ^ The bitness of the traced child
                 -> String         -- ^ System call name
                 -> IO (Maybe Int) -- ^ Just @scno@ if system call name is valid, 'Nothing' otherwise
lookupSystemCall bit scname
    | bit == Bitness32 && not bitness32Supported = error $ "lookupSystemCall: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "lookupSystemCall: unsupported bitness " ++ show bit
    | otherwise = withCString scname $ \ptr -> do
        let ret = pink_name_lookup ptr bit'
        if ret < 0
            then return Nothing
            else return $ Just (fromIntegral ret)
    where
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit

{-|
    Returns the last system call number called by the traced child.

    * Note: This function calls 'throwErrno' in case of failure.
-}
getSystemCallNumber :: Bitness   -- ^ Bitness of the traced child
                    -> ProcessID -- ^ Process ID of the traced child
                    -> IO Int    -- ^ The number of the system call
getSystemCallNumber bit pid
    | bit == Bitness32 && not bitness32Supported = error $ "getSystemCallNumber: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "getSystemCallNumber: unsupported bitness " ++ show bit
    | otherwise = alloca $ \ptr -> do
        ret <- pink_util_get_syscall pid bit' ptr
        fmap fromIntegral $ if ret == 0 then throwErrno "pink_util_get_syscall" else peek ptr
    where
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit

{-|
    Sets the number of the last system call called by the traced child.

    * Note: This function calls 'throwErrno' in case of failure.
-}
setSystemCallNumber :: Int       -- ^ The number of the system call
                    -> Bitness   -- ^ Bitness of the traced child
                    -> ProcessID -- ^ Process ID of the traced child
                    -> IO ()
setSystemCallNumber scno bit pid
    | bit == Bitness32 && not bitness32Supported = error $ "setSystemCallNumber: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "setSystemCallNumber: unsupported bitness " ++ show bit
    | otherwise = do
        ret <- pink_util_set_syscall pid bit' scno'
        when (ret == 0) (throwErrno "pink_util_set_syscall")
    where
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit
        scno' :: CLong
        scno' = fromIntegral scno

{-|
    Returns the return value of the last system call called by the traced child.

    * Note: This function calls 'throwErrno' in case of failure.
-}
getSystemCallReturn :: ProcessID           -- ^ Process ID of the traced child
                    -> IO SystemCallReturn -- ^ The return value
getSystemCallReturn pid = alloca $ \ptr -> do
    ret <- pink_util_get_return pid ptr
    if ret == 0 then throwErrno "pink_util_get_return" else peek ptr

{-|
    Sets the return value of the last system call called by the traced child.

    * Note: This function calls 'throwErrno' in case of failure.
-}
setSystemCallReturn :: SystemCallReturn -- ^ The return value
                    -> ProcessID        -- ^ Process ID of the traced child
                    -> IO ()
setSystemCallReturn ret pid = do
    res <- pink_util_set_return pid ret
    when (res == 0) (throwErrno "pink_util_set_return")

{-|
    Returns the system call argument at the given index.

    * Note: This function calls 'throwErrno' in case of failure.
-}
getSystemCallArgument :: Index     -- ^ The index of the argument
                      -> Bitness   -- ^ The bitness of the traced child
                      -> ProcessID -- ^ Process ID of the traced child
                      -> IO Int    -- ^ The value of the argument
getSystemCallArgument index bit pid
    | bit == Bitness32 && not bitness32Supported = error $ "getSystemCallArgument: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "getSystemCallArgument: unsupported bitness " ++ show bit
    | otherwise = alloca $ \ptr -> do
        ret <- pink_util_get_arg pid bit' index' ptr
        fmap fromIntegral $ if ret == 0 then throwErrno "pink_util_get_arg" else peek ptr
    where
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit
        index' :: CUInt
        index' = (fromIntegral . fromEnum) index
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
