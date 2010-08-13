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
{-| Module:      System.PinkTrace.String
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: portable, ffi

    Pink's string decoding and encoding functions
-}
--}}}
--{{{ Exports
module System.PinkTrace.String
    ( decode
    , encode
    , encode_safe
    ) where
--}}}
--{{{ Includes
#include <stdlib.h>
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Foreign.C.Error       (throwErrno)
import Foreign.C.String      (CString, peekCString, withCStringLen)
import Foreign.C.Types       (CInt, CUInt, CSize)
import Foreign.Ptr           (nullPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import System.Posix.Types    (CPid)

import System.PinkTrace.Bitness ( Bitness(..)
                                , bitness32Supported
                                , bitness64Supported
                                )
--}}}
--{{{ Types
type Pid    = Int
type Index  = Int
type Length = Int
--}}}
--{{{ Functions
foreign import ccall "free" c_free :: CString -> IO ()
foreign import ccall pink_decode_string :: CPid -> CInt -> CUInt -> CString -> CSize -> IO CInt
foreign import ccall pink_decode_string_persistent :: CPid -> CInt -> CUInt -> IO CString
decode :: Pid -> Bitness -> Index -> Length -> IO String
decode pid bit index len
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "decode: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "decode: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "decode: unsupported bitness " ++ show bit
    | len < 0   = do
        str  <- pink_decode_string_persistent pid' bit' index'
        str' <- if str == nullPtr
            then throwErrno "pink_decode_string_persistent"
            else peekCString str
        c_free str >> return str'
    | otherwise = allocaBytes (len * #{size char}) $ \ptr -> do
        ret <- pink_decode_string pid' bit' index' ptr len'
        if ret == 0
            then throwErrno "pink_decode_string"
            else peekCString ptr
    where
        pid' :: CPid
        pid' = fromIntegral pid
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index
        len' :: CSize
        len' = fromIntegral len

foreign import ccall pink_encode_simple :: CPid -> CInt -> CUInt -> CString -> CSize -> IO CInt
encode :: Pid -> Bitness -> Index -> String -> IO ()
encode pid bit index src
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "decode: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "decode: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "decode: unsupported bitness " ++ show bit
    | otherwise = withCStringLen src $ \(s, l) -> do
        ret <- pink_encode_simple pid' bit' index' s $ fromIntegral l
        if ret == 0
            then throwErrno "pink_encode_simple"
            else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index

#ifdef PINKTRACE_LINUX
foreign import ccall pink_encode_simple_safe :: CPid -> CInt -> CUInt -> CString -> CSize -> IO CInt
encode_safe :: Pid -> Bitness -> Index -> String -> IO ()
encode_safe pid bit index src
    | index < 0 || index >= #{const PINK_MAX_INDEX} = error $ "encode: invalid index " ++ show index
    | bit == Bitness32 && not bitness32Supported = error $ "encode: unsupported bitness " ++ show bit
    | bit == Bitness64 && not bitness64Supported = error $ "encode: unsupported bitness " ++ show bit
    | otherwise = withCStringLen src $ \(s, l) -> do
        ret <- pink_encode_simple_safe pid' bit' index' s $ fromIntegral l
        if ret == 0
            then throwErrno "pink_encode_simple"
            else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
        index' :: CUInt
        index' = fromIntegral index
#else
encode_safe :: Pid -> Bitness -> Index -> String -> IO ()
encode_safe _ _ _ _ = error "encode_safe: not implemented"
#endif
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
