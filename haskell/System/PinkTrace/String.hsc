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
    Module:      System.PinkTrace.String
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: non-portable, requires freebsd or linux

    Pink's string decoding and encoding functions
-}
module System.PinkTrace.String
    ( decode
    , encode
    , encodeSafe
    ) where
--}}}
--{{{ Includes
#include <stdlib.h>
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Control.Monad         (when)
import Foreign.C.Error       (throwErrno)
import Foreign.C.String      (CString, peekCString, withCStringLen)
import Foreign.C.Types       (CInt, CUInt, CSize)
import Foreign.Ptr           (nullPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import System.Posix.Types    (CPid, ProcessID)

import System.PinkTrace         (Index(..))
import System.PinkTrace.Bitness ( Bitness(..)
                                , bitness32Supported
                                , bitness64Supported
                                )
--}}}
--{{{ Foreign Imports
foreign import ccall "free" c_free :: CString -> IO ()
foreign import ccall pink_decode_string :: CPid -> CInt -> CUInt -> CString -> CSize -> IO CInt
foreign import ccall pink_decode_string_persistent :: CPid -> CInt -> CUInt -> IO CString
foreign import ccall pink_encode_simple :: CPid -> CInt -> CUInt -> CString -> CSize -> IO CInt
#ifdef PINKTRACE_LINUX
foreign import ccall pink_encode_simple_safe :: CPid -> CInt -> CUInt -> CString -> CSize -> IO CInt
#endif
--}}}
--{{{ Functions
{-|
    This function decodes the string at the argument of the given index.

    * Note: This function calls 'throwErrno' in case of failure.
-}
decode :: Index     -- ^ The index of the argument
       -> Int       -- ^ Max length of the string
                    --   If smaller than zero, @pinktrace@ tries to determine the string length.
       -> Bitness   -- ^ The bitness of the traced child
       -> ProcessID -- ^ Process ID of the traced child
       -> IO String -- ^ The decoded string
decode index len bit pid
    | bit == Bitness32 && not bitness32Supported = error $ "decode: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "decode: unsupported bitness " ++ show bit
    | len < 0   = do
        str  <- pink_decode_string_persistent pid bit' index'
        str' <- if str == nullPtr
            then throwErrno "pink_decode_string_persistent"
            else peekCString str
        c_free str >> return str'
    | otherwise = allocaBytes (len * #{size char}) $ \ptr -> do
        ret <- pink_decode_string pid bit' index' ptr len'
        if ret == 0
            then throwErrno "pink_decode_string"
            else peekCString ptr
    where
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit
        index' :: CUInt
        index' = (fromIntegral . fromEnum) index
        len' :: CSize
        len' = fromIntegral len

{-|
    Encode a string into the argument of the given index.

    * Note: This function calls 'throwErrno' in case of failure.

    * Warning: Care must be taken when using this function as unexpected things may happen.
-}
encode :: Index     -- ^ The index of the argument
       -> String    -- ^ The string to be encoded
       -> Bitness   -- ^ The bitness of the traced child
       -> ProcessID -- ^ Process ID of the traced child
       -> IO ()
encode index src bit pid
    | bit == Bitness32 && not bitness32Supported = error $ "encode: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "encode: unsupported bitness " ++ show bit
    | otherwise = withCStringLen src $ \(s, l) -> do
        ret <- pink_encode_simple pid bit' index' s $ fromIntegral l
        when (ret == 0) (throwErrno "pink_encode_simple")
    where
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit
        index' :: CUInt
        index' = (fromIntegral . fromEnum) index

#ifdef PINKTRACE_LINUX
{-|
    Encode a string into the argument of the given index with additional
    checking for writable areas.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux

    * Warning: Care must be taken when using this function as unexpected things may happen.
-}
encodeSafe :: Index     -- ^ The index of the argument
           -> String    -- ^ The string to be encoded
           -> Bitness   -- ^ The bitness of the traced child
           -> ProcessID -- ^ Process ID of the traced child
           -> IO ()
encodeSafe index src bit pid
    | bit == Bitness32 && not bitness32Supported = error $ "encodeSafe: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "encodeSafe: unsupported bitness " ++ show bit
    | otherwise = withCStringLen src $ \(s, l) -> do
        ret <- pink_encode_simple_safe pid bit' index' s $ fromIntegral l
        when (ret == 0) (throwErrno "pink_encode_simple_safe")
    where
        bit' :: CInt
        bit' = (fromIntegral . fromEnum) bit
        index' :: CUInt
        index' = (fromIntegral . fromEnum) index
#else
{-|
    Encode a string into the argument of the given index with additional
    checking for writable areas.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux

    * Warning: Care must be taken when using this function as unexpected things may happen.
-}
encodeSafe :: Index     -- ^ The index of the argument
           -> String    -- ^ The string to be encoded
           -> Bitness   -- ^ The bitness of the traced child
           -> ProcessID -- ^ Process ID of the traced child
           -> IO ()
encodeSafe _ _ _ _ = error "encodeSafe: not implemented"
#endif
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
