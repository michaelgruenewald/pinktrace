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

--{{{ Description
{-| Module:      System.PinkTrace.SysCall
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: portable, ffi

    Pink's system call utility functions
-}
--}}}
--{{{ Exports
module System.PinkTrace.SysCall
    ( name
    , lookup
    , getNo
    , setNo
    , getRet
    , setRet
    , getArg
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Prelude hiding        (lookup)
import Foreign.C.Error       (throwErrno)
import Foreign.C.String      (CString, peekCString, withCString)
import Foreign.C.Types       (CInt, CLong, CUInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr           (Ptr, nullPtr)
import Foreign.Storable      (peek)
import System.Posix.Types    (CPid, ProcessID)

import System.PinkTrace         (Index)
import System.PinkTrace.Bitness ( Bitness(..)
                                , bitness32Supported
                                , bitness64Supported
                                )
--}}}
--{{{ Functions
foreign import ccall pink_name_syscall :: CLong -> CInt -> CString
name :: Int -> Bitness -> IO (Maybe String)
name scno bit
    | bit == Bitness32 && not bitness32Supported = error $ "name: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "name: unsupported bitness " ++ show bit
    | otherwise = if scname == nullPtr then return Nothing else fmap Just (peekCString scname)
    where
        scno' :: CLong
        scno' = fromIntegral scno
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        scname :: CString
        scname = pink_name_syscall scno' bit'

foreign import ccall pink_name_lookup :: CString -> CInt -> CLong
lookup :: String -> Bitness -> IO (Maybe Int)
lookup scname bit
    | bit == Bitness32 && not bitness32Supported = error $ "lookup: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "lookup: unsupported bitness " ++ show bit
    | otherwise = withCString scname $ \ptr -> do
        let ret = pink_name_lookup ptr bit'
        if ret < 0
            then return Nothing
            else return $ Just (fromIntegral ret)
    where
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit

foreign import ccall pink_util_get_syscall :: CPid -> CInt -> Ptr CLong -> IO CInt
getNo :: ProcessID -> Bitness -> IO Int
getNo pid bit
    | bit == Bitness32 && not bitness32Supported = error $ "getNo: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "getNo: unsupported bitness " ++ show bit
    | otherwise = alloca $ \ptr -> do
        ret <- pink_util_get_syscall pid bit' ptr
        fmap fromIntegral $ if ret == 0 then throwErrno "pink_util_get_syscall" else peek ptr
    where
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit

foreign import ccall pink_util_set_syscall :: CPid -> CInt -> CLong -> IO CInt
setNo :: ProcessID -> Bitness -> Int -> IO ()
setNo pid bit scno
    | bit == Bitness32 && not bitness32Supported = error $ "setNo: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "setNo: unsupported bitness " ++ show bit
    | otherwise = do
        ret <- pink_util_set_syscall pid bit' scno'
        if ret == 0
            then throwErrno "pink_util_set_syscall"
            else return ()
    where
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        scno' :: CLong
        scno' = fromIntegral scno

foreign import ccall pink_util_get_return :: CPid -> Ptr CLong -> IO CInt
getRet :: ProcessID -> IO Int
getRet pid = alloca $ \ptr -> do
    ret <- pink_util_get_return pid ptr
    fmap fromIntegral $ if ret == 0 then throwErrno "pink_util_get_return" else peek ptr

foreign import ccall pink_util_set_return :: CPid -> CLong -> IO CInt
setRet :: ProcessID -> Int -> IO ()
setRet pid ret = do
    res <- pink_util_set_return pid ret'
    if res == 0
        then throwErrno "pink_util_set_return"
        else return ()
    where
        ret' :: CLong
        ret' = fromIntegral ret

foreign import ccall pink_util_get_arg :: CPid -> CInt -> CUInt -> Ptr CLong -> IO CInt
getArg :: ProcessID -> Bitness -> Index -> IO Int
getArg pid bit index
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "getArg: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "getArg: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "getArg: unsupported bitness " ++ show bit
    | otherwise = alloca $ \ptr -> do
        ret <- pink_util_get_arg pid bit' index' ptr
        fmap fromIntegral $ if ret == 0 then throwErrno "pink_util_get_arg" else peek ptr
    where
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
