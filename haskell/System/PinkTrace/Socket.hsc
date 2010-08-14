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
{-| Module:      System.PinkTrace.Socket
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: portable, ffi

    Pink's socket decoding functions
-}
--}}}
--{{{ Exports
module System.PinkTrace.Socket
    ( Address(..)
    , SubCall(..)
    , name
    , decodeAddress
    , decodeAddressFd
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Foreign.C.Error       (throwErrno)
import Foreign.C.Types       (CInt, CUInt, CLong)
import Foreign.ForeignPtr    (ForeignPtr)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Ptr           (Ptr, nullPtr)
import Foreign.Storable      (peek)
import System.Posix.Types    (CPid)

import System.PinkTrace         (Index, Pid)
import System.PinkTrace.Bitness ( Bitness(..)
                                , bitness32Supported
                                , bitness64Supported
                                )
--}}}
--{{{ Types
newtype Address = Address (ForeignPtr Address)
data SubCall = SubCall_Socket
    | SubCall_Bind
    | SubCall_Connect
    | SubCall_Listen
    | SubCall_Accept
    | SubCall_Getsockname
    | SubCall_Getpeername
    | SubCall_Socketpair
    | SubCall_Send
    | SubCall_Recv
    | SubCall_Sendto
    | SubCall_Recvfrom
    | SubCall_Shutdown
    | SubCall_Setsockopt
    | SubCall_Getsockopt
    | SubCall_Sendmsg
    | SubCall_Recvmsg
    | SubCall_Accept4
    deriving (Eq,Show)
#ifdef PINKTRACE_LINUX
instance Enum SubCall where
    fromEnum SubCall_Socket      = #{const PINK_SOCKET_SUBCALL_SOCKET}
    fromEnum SubCall_Bind        = #{const PINK_SOCKET_SUBCALL_BIND}
    fromEnum SubCall_Connect     = #{const PINK_SOCKET_SUBCALL_CONNECT}
    fromEnum SubCall_Listen      = #{const PINK_SOCKET_SUBCALL_LISTEN}
    fromEnum SubCall_Accept      = #{const PINK_SOCKET_SUBCALL_ACCEPT}
    fromEnum SubCall_Getsockname = #{const PINK_SOCKET_SUBCALL_GETSOCKNAME}
    fromEnum SubCall_Getpeername = #{const PINK_SOCKET_SUBCALL_GETPEERNAME}
    fromEnum SubCall_Socketpair  = #{const PINK_SOCKET_SUBCALL_SOCKETPAIR}
    fromEnum SubCall_Send        = #{const PINK_SOCKET_SUBCALL_SEND}
    fromEnum SubCall_Recv        = #{const PINK_SOCKET_SUBCALL_RECV}
    fromEnum SubCall_Sendto      = #{const PINK_SOCKET_SUBCALL_SENDTO}
    fromEnum SubCall_Recvfrom    = #{const PINK_SOCKET_SUBCALL_RECVFROM}
    fromEnum SubCall_Shutdown    = #{const PINK_SOCKET_SUBCALL_SHUTDOWN}
    fromEnum SubCall_Setsockopt  = #{const PINK_SOCKET_SUBCALL_SETSOCKOPT}
    fromEnum SubCall_Getsockopt  = #{const PINK_SOCKET_SUBCALL_GETSOCKOPT}
    fromEnum SubCall_Sendmsg     = #{const PINK_SOCKET_SUBCALL_SENDMSG}
    fromEnum SubCall_Recvmsg     = #{const PINK_SOCKET_SUBCALL_RECVMSG}
    fromEnum SubCall_Accept4     = #{const PINK_SOCKET_SUBCALL_ACCEPT4}

    toEnum #{const PINK_SOCKET_SUBCALL_SOCKET}      = SubCall_Socket
    toEnum #{const PINK_SOCKET_SUBCALL_BIND}        = SubCall_Bind
    toEnum #{const PINK_SOCKET_SUBCALL_CONNECT}     = SubCall_Connect
    toEnum #{const PINK_SOCKET_SUBCALL_LISTEN}      = SubCall_Listen
    toEnum #{const PINK_SOCKET_SUBCALL_ACCEPT}      = SubCall_Accept
    toEnum #{const PINK_SOCKET_SUBCALL_GETSOCKNAME} = SubCall_Getsockname
    toEnum #{const PINK_SOCKET_SUBCALL_GETPEERNAME} = SubCall_Getpeername
    toEnum #{const PINK_SOCKET_SUBCALL_SOCKETPAIR}  = SubCall_Socketpair
    toEnum #{const PINK_SOCKET_SUBCALL_SEND}        = SubCall_Send
    toEnum #{const PINK_SOCKET_SUBCALL_RECV}        = SubCall_Recv
    toEnum #{const PINK_SOCKET_SUBCALL_SENDTO}      = SubCall_Sendto
    toEnum #{const PINK_SOCKET_SUBCALL_RECVFROM}    = SubCall_RecvFrom
    toEnum #{const PINK_SOCKET_SUBCALL_SHUTDOWN}    = SubCall_Shutdown
    toEnum #{const PINK_SOCKET_SUBCALL_SETSOCKOPT}  = SubCall_Setsockopt
    toEnum #{const PINK_SOCKET_SUBCALL_GETSOCKOPT}  = SubCall_Getsockopt
    toEnum #{const PINK_SOCKET_SUBCALL_SENDMSG}     = SubCall_Sendmsg
    toEnum #{const PINK_SOCKET_SUBCALL_RECVMSG}     = SubCall_Recvmsg
    toEnum #{const PINK_SOCKET_SUBCALL_ACCEPT4}     = SubCall_Accept4
    toEnum unmatched                                = error $ "SubCall.toEnum: Cannot match " ++ show unmatched
#endif
--}}}
--{{{ Functions
#ifdef PINKTRACE_LINUX
foreign import ccall pink_name_socket_subcall :: CInt -> CString
name :: SubCall -> IO String
name scall = peekCString $ pink_name_socket_subcall scall'
    where
        scall' = fromIntegral $ fromEnum scall
#else
name :: SubCall -> IO String
name _ = error "name: not implemented"
#endif

foreign import ccall pink_decode_socket_address :: CPid -> CInt -> CUInt -> Ptr CLong -> Ptr Address -> IO CInt
decodeAddress :: Pid -> Bitness -> Index -> IO (Ptr Address)
decodeAddress pid bit index
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "decodeAddress: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "decodeAddress: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "decodeAddress: unsupported bitness " ++ show bit
    | otherwise = do
        ptr <- mallocBytes #{size pink_socket_address_t}
        ret <- pink_decode_socket_address pid' bit' index' nullPtr ptr
        if ret == 0
            then free ptr >> throwErrno "pink_decode_socket_address"
            else return ptr
    where
        pid' :: CPid
        pid' = fromIntegral pid
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index

decodeAddressFd :: Pid -> Bitness -> Index -> IO (Int, Ptr Address)
decodeAddressFd pid bit index
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "decodeAddressFd: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "decodeAddressFd: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "decodeAddressFd: unsupported bitness " ++ show bit
    | otherwise = do
        ptr <- mallocBytes #{size pink_socket_address_t}
        alloca $ \fptr -> do
            ret <- pink_decode_socket_address pid' bit' index' fptr ptr
            if ret == 0
                then free ptr >> throwErrno "pink_decode_socket_address"
                else do
                    fd <- peek fptr
                    return (fromIntegral fd, ptr)
    where
        pid' :: CPid
        pid' = fromIntegral pid
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
