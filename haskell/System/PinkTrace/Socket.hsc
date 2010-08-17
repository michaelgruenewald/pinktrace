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
    Module:      System.PinkTrace.Socket
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: non-portable, requires freebsd or linux

    Pink's socket decoding functions
-}
module System.PinkTrace.Socket
    ( Address(..)
    , SubCall(..)
    , nameSocketSubCall
    , decodeSocketAddress
    , decodeSocketAddressFd
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
import System.Posix.Types    (CPid, ProcessID)

#ifdef PINKTRACE_LINUX
import Foreign.C.String      (CString, peekCString)
#endif

import System.PinkTrace         (Index)
import System.PinkTrace.Bitness ( Bitness(..)
                                , bitness32Supported
                                , bitness64Supported
                                )
--}}}
--{{{ Foreign Imports
#ifdef PINKTRACE_LINUX
foreign import ccall pink_name_socket_subcall :: CInt -> CString
#endif
foreign import ccall pink_decode_socket_address :: CPid -> CInt -> CUInt -> Ptr CLong -> Ptr Address -> IO CInt
--}}}
--{{{ Types
-- |This type represents a decoded socket address.
newtype Address = Address (ForeignPtr Address)
-- |Socket subcalls
data SubCall = Socket -- ^ socket() subcall
    | Bind            -- ^ bind() subcall
    | Connect         -- ^ connect() subcall
    | Listen          -- ^ listen() subcall
    | Accept          -- ^ accept() subcall
    | Getsockname     -- ^ getsockname() subcall
    | Getpeername     -- ^ getpeername() subcall
    | Socketpair      -- ^ socketpair() subcall
    | Send            -- ^ send() subcall
    | Recv            -- ^ recv() subcall
    | Sendto          -- ^ sendto() subcall
    | Recvfrom        -- ^ recvfrom() subcall
    | Shutdown        -- ^ shutdown() subcall
    | Setsockopt      -- ^ setsockopt() subcall
    | Getsockopt      -- ^ getsockopt() subcall
    | Sendmsg         -- ^ sendmsg() subcall
    | Recvmsg         -- ^ recvmsg() subcall
    | Accept4         -- ^ accept4() subcall
    deriving (Eq,Show)
#ifdef PINKTRACE_LINUX
instance Enum SubCall where
    fromEnum Socket      = #{const PINK_SOCKET_SUBCALL_SOCKET}
    fromEnum Bind        = #{const PINK_SOCKET_SUBCALL_BIND}
    fromEnum Connect     = #{const PINK_SOCKET_SUBCALL_CONNECT}
    fromEnum Listen      = #{const PINK_SOCKET_SUBCALL_LISTEN}
    fromEnum Accept      = #{const PINK_SOCKET_SUBCALL_ACCEPT}
    fromEnum Getsockname = #{const PINK_SOCKET_SUBCALL_GETSOCKNAME}
    fromEnum Getpeername = #{const PINK_SOCKET_SUBCALL_GETPEERNAME}
    fromEnum Socketpair  = #{const PINK_SOCKET_SUBCALL_SOCKETPAIR}
    fromEnum Send        = #{const PINK_SOCKET_SUBCALL_SEND}
    fromEnum Recv        = #{const PINK_SOCKET_SUBCALL_RECV}
    fromEnum Sendto      = #{const PINK_SOCKET_SUBCALL_SENDTO}
    fromEnum Recvfrom    = #{const PINK_SOCKET_SUBCALL_RECVFROM}
    fromEnum Shutdown    = #{const PINK_SOCKET_SUBCALL_SHUTDOWN}
    fromEnum Setsockopt  = #{const PINK_SOCKET_SUBCALL_SETSOCKOPT}
    fromEnum Getsockopt  = #{const PINK_SOCKET_SUBCALL_GETSOCKOPT}
    fromEnum Sendmsg     = #{const PINK_SOCKET_SUBCALL_SENDMSG}
    fromEnum Recvmsg     = #{const PINK_SOCKET_SUBCALL_RECVMSG}
    fromEnum Accept4     = #{const PINK_SOCKET_SUBCALL_ACCEPT4}

    toEnum #{const PINK_SOCKET_SUBCALL_SOCKET}      = Socket
    toEnum #{const PINK_SOCKET_SUBCALL_BIND}        = Bind
    toEnum #{const PINK_SOCKET_SUBCALL_CONNECT}     = Connect
    toEnum #{const PINK_SOCKET_SUBCALL_LISTEN}      = Listen
    toEnum #{const PINK_SOCKET_SUBCALL_ACCEPT}      = Accept
    toEnum #{const PINK_SOCKET_SUBCALL_GETSOCKNAME} = Getsockname
    toEnum #{const PINK_SOCKET_SUBCALL_GETPEERNAME} = Getpeername
    toEnum #{const PINK_SOCKET_SUBCALL_SOCKETPAIR}  = Socketpair
    toEnum #{const PINK_SOCKET_SUBCALL_SEND}        = Send
    toEnum #{const PINK_SOCKET_SUBCALL_RECV}        = Recv
    toEnum #{const PINK_SOCKET_SUBCALL_SENDTO}      = Sendto
    toEnum #{const PINK_SOCKET_SUBCALL_RECVFROM}    = Recvfrom
    toEnum #{const PINK_SOCKET_SUBCALL_SHUTDOWN}    = Shutdown
    toEnum #{const PINK_SOCKET_SUBCALL_SETSOCKOPT}  = Setsockopt
    toEnum #{const PINK_SOCKET_SUBCALL_GETSOCKOPT}  = Getsockopt
    toEnum #{const PINK_SOCKET_SUBCALL_SENDMSG}     = Sendmsg
    toEnum #{const PINK_SOCKET_SUBCALL_RECVMSG}     = Recvmsg
    toEnum #{const PINK_SOCKET_SUBCALL_ACCEPT4}     = Accept4
    toEnum unmatched                                = error $ "SubCall.toEnum: Cannot match " ++ show unmatched
#endif
--}}}
--{{{ Functions
#ifdef PINKTRACE_LINUX
{-|
    Returns the name of the socket subcall.

    * Availability: Linux
-}
nameSocketSubCall :: SubCall -> IO String
nameSocketSubCall scall = peekCString $ pink_name_socket_subcall scall'
    where
        scall' :: CInt
        scall' = fromIntegral $ fromEnum scall
#else
{-|
    Returns the name of the socket subcall.

    * Availability: Linux
-}
nameSocketSubCall :: SubCall -> IO String
nameSocketSubCall _ = error "nameSocketSubCall: not implemented"
#endif

{-|
    Decodes the socket address at the given argument index.

    * Note: This function decodes the @socketcall(2)@ system call on some
      architectures.
-}
decodeSocketAddress :: ProcessID        -- ^ Process ID of the traced child
                    -> Bitness          -- ^ The bitness of the traced child
                    -> Index            -- ^ The index of the argument
                    -> IO (Ptr Address) -- ^ The decoded socket address
decodeSocketAddress pid bit index
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "decodeSocketAddress: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "decodeSocketAddress: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "decodeSocketAddress: unsupported bitness " ++ show bit
    | otherwise = do
        ptr <- mallocBytes #{size pink_socket_address_t}
        ret <- pink_decode_socket_address pid bit' index' nullPtr ptr
        if ret == 0
            then free ptr >> throwErrno "pink_decode_socket_address"
            else return ptr
    where
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index

{-|
    Decodes the socket address at the given argument index; and the file
    descriptor at index 0.

    * Note: This function decodes the @socketcall(2)@ system call on some
      architectures.
-}
decodeSocketAddressFd :: ProcessID              -- ^ Process ID of the traced child
                      -> Bitness                -- ^ The bitness of the traced child
                      -> Index                  -- ^ The index of the argument
                      -> IO (CInt, Ptr Address) -- ^ Decoded socket file descriptor and the socket address
decodeSocketAddressFd pid bit index
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "decodeSocketAddressFd: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "decodeSocketAddressFd: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "decodeSocketAddressFd: unsupported bitness " ++ show bit
    | otherwise = do
        ptr <- mallocBytes #{size pink_socket_address_t}
        alloca $ \fptr -> do
            ret <- pink_decode_socket_address pid bit' index' fptr ptr
            if ret == 0
                then free ptr >> throwErrno "pink_decode_socket_address"
                else do
                    fd <- peek fptr
                    return (fromIntegral fd, ptr)
    where
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
