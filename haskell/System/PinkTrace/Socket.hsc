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
    ( Address
    , SubCall(..)
    , Family(..)
    , nameSocketSubCall
    , decodeSocketAddress
    , decodeSocketAddressFd
    , freeSocketAddress
    , familyOfSocketAddress
    , isAbstractUNIXSocketAddress
    , pathOfUNIXSocketAddress
    , ipOfInetSocketAddress
    , ipOfInet6SocketAddress
    ) where
--}}}
--{{{ Includes
#include <netinet/in.h>
#include <pinktrace/pink.h>
#include "HsSocket.h"

#ifndef INET_ADDRSTRLEN
#define INET_ADDRSTRLEN 16
#endif

#if PINKTRACE_HAVE_IPV6
#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif
#endif

--}}}
--{{{ Imports
import Foreign.C.Error       (throwErrno)
import Foreign.C.Types       (CInt, CUInt, CLong)
import Foreign.ForeignPtr    (ForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, mallocBytes)
import Foreign.Ptr           (Ptr, nullPtr)
import Foreign.Storable      (peek)
import System.Posix.Types    (CPid, ProcessID)

#ifdef PINKTRACE_LINUX
import Foreign.C.String      (CString, peekCString)
#endif

import System.PinkTrace         (Index(..))
import System.PinkTrace.Bitness ( Bitness(..)
                                , bitness32Supported
                                , bitness64Supported
                                )
--}}}
--{{{ Foreign Imports
#ifdef PINKTRACE_LINUX
foreign import ccall pink_name_socket_subcall :: CInt -> CString
#endif
foreign import ccall pink_decode_socket_address :: CPid -> CInt -> CUInt -> Ptr CLong -> Address -> IO CInt
foreign import ccall "__pinkhs_socket_family" c_socket_family :: Address -> CInt
foreign import ccall "__pinkhs_socket_isabstract" c_socket_isabstract :: Address -> CInt
foreign import ccall "__pinkhs_socket_path" c_socket_path :: Address -> CString
foreign import ccall "__pinkhs_socket_inet_ntop" c_socket_inet_ntop :: Address -> CString -> CString
#if PINKTRACE_HAVE_IPV6
foreign import ccall "__pinkhs_socket_inet_ntop6" c_socket_inet_ntop6 :: Address -> CString -> CString
#endif
--}}}
--{{{ Types
-- |This type represents a decoded socket address.
newtype PinkSocketAddress = PinkSocketAddress (ForeignPtr PinkSocketAddress)
type Address = Ptr PinkSocketAddress

-- |Socket families
data Family = AF_NULL -- ^ NULL
    | AF_UNIX         -- ^ Unix
    | AF_INET         -- ^ IPV4
#if PINKTRACE_HAVE_IPV6
    | AF_INET6        -- ^ IPV6
#endif
#if PINKTRACE_HAVE_NETLINK
    | AF_NETLINK      -- ^ Netlink
#endif
    deriving (Eq,Show)
instance Enum Family where
    fromEnum AF_NULL    = -1
    fromEnum AF_UNIX    = #{const AF_UNIX}
    fromEnum AF_INET    = #{const AF_INET}
#if PINKTRACE_HAVE_IPV6
    fromEnum AF_INET6   = #{const AF_INET6}
#endif
#if PINKTRACE_HAVE_NETLINK
    fromEnum AF_NETLINK = #{const AF_NETLINK}
#endif

    toEnum (-1)                = AF_NULL
    toEnum #{const AF_UNIX}    = AF_UNIX
    toEnum #{const AF_INET}    = AF_INET
#if PINKTRACE_HAVE_IPV6
    toEnum #{const AF_INET6}   = AF_INET6
#endif
#if PINKTRACE_HAVE_NETLINK
    toEnum #{const AF_NETLINK} = AF_NETLINK
#endif
    toEnum unmatched           = error $ "SubCall.toEnum: Cannot match " ++ show unmatched

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
        scall' = (fromIntegral . fromEnum) scall
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
decodeSocketAddress :: Index      -- ^ The index of the argument
                    -> Bitness    -- ^ The bitness of the traced child
                    -> ProcessID  -- ^ Process ID of the traced child
                    -> IO Address -- ^ The decoded socket address
decodeSocketAddress index bit pid
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
        bit' = (fromIntegral . fromEnum) bit
        index' :: CUInt
        index' = (fromIntegral . fromEnum) index

{-|
    Decodes the socket address at the given argument index; and the file
    descriptor at index 0.

    * Note: This function decodes the @socketcall(2)@ system call on some
      architectures.
-}
decodeSocketAddressFd :: Index              -- ^ The index of the argument
                      -> Bitness            -- ^ The bitness of the traced child
                      -> ProcessID          -- ^ Process ID of the traced child
                      -> IO (CInt, Address) -- ^ Decoded socket file descriptor and the socket address
decodeSocketAddressFd index bit pid
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
        bit' = (fromIntegral . fromEnum) bit
        index' :: CUInt
        index' = (fromIntegral . fromEnum) index

-- |Free the block of memory allocated for 'Address'
freeSocketAddress :: Address -> IO ()
freeSocketAddress = free

-- |Returns the family of the decoded socket 'Address'
familyOfSocketAddress :: Address -> Family
familyOfSocketAddress = toEnum . fromIntegral . c_socket_family

-- |Returns True if the 'Address' is an abstract UNIX socket 'Address'
isAbstractUNIXSocketAddress :: Address -> Bool
isAbstractUNIXSocketAddress ptr = ret /= 0
    where
        ret :: Int
        ret = (fromIntegral . c_socket_isabstract) ptr

-- |Returns the path of the UNIX socket 'Address'
pathOfUNIXSocketAddress :: Address -> IO FilePath
pathOfUNIXSocketAddress ptr
    | familyOfSocketAddress ptr /= AF_UNIX = error $ "pathOfUNIXSocketAddress: invalid family" ++ show (familyOfSocketAddress ptr)
    | otherwise = (peekCString . c_socket_path) ptr

-- |Returns the IP address of the Inet socket 'Address' as a 'String'
ipOfInetSocketAddress :: Address -> IO String
ipOfInetSocketAddress ptr
    | familyOfSocketAddress ptr /= AF_INET = error $ "ipOfInetSocketAddress: invalid family" ++ show (familyOfSocketAddress ptr)
    | otherwise = allocaBytes (#{const INET_ADDRSTRLEN} * #{size char}) $ \str -> do
        let str' = c_socket_inet_ntop ptr str
        peekCString str'


#if PINKTRACE_HAVE_IPV6
{-|
    Returns teh IP address of the Inet6 socket 'Address' as a 'String'

    * Availability: Only available if PinkTrace was compiled with IPV6 support.
-}
ipOfInet6SocketAddress :: Address -> IO String
ipOfInet6SocketAddress ptr
    | familyOfSocketAddress ptr /= AF_INET6 = error $ "ipOfInet6SocketAddress: invalid family" ++ show (familyOfSocketAddress ptr)
    | otherwise = allocaBytes (#{const INET6_ADDRSTRLEN} * #{size char}) $ \str -> do
        let str' = c_socket_inet_ntop6 ptr str
        peekCString str'
#else
{-|
    Returns teh IP address of the Inet6 socket 'Address' as a 'String'

    * Availability: Only available if PinkTrace was compiled with IPV6 support.
-}
ipOfInet6SocketAddress :: Address -> IO String
ipOfInet6SocketAddress _ = error "ipOfInet6SocketAddress: not implemented"
#endif

--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
