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
{-| Module:      System.PinkTrace.Bitness
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: portable, ffi

    Pink's bitness modes
-}
--}}}
--{{{ Exports
module System.PinkTrace.Bitness
    ( bitness32
    , bitness64
    , bitnessDefault
    , bitnessSupported
    , get
    , name
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Foreign.C.Error    (throwErrno)
import Foreign.C.String   (CString, peekCString)
import Foreign.C.Types    (CInt)
import System.Posix.Types (CPid)
--}}}
--{{{ Types
type Pid     = Int
type Bitness = Int
--}}}
--{{{ Functions
bitness32 :: Int
bitness32 = #{const PINK_BITNESS_32}

bitness64 :: Int
bitness64 = #{const PINK_BITNESS_64}

bitnessDefault :: Int
bitnessDefault = #{const PINKTRACE_DEFAULT_BITNESS}

bitnessSupported :: Int
bitnessSupported = #{const PINKTRACE_SUPPORTED_BITNESS}

foreign import ccall pink_bitness_get :: CPid -> IO CInt
get :: Pid -> IO Bitness
get pid = do
    ret <- pink_bitness_get pid'
    if ret == #{const PINK_BITNESS_UNKNOWN}
        then throwErrno "pink_bitness_get"
        else return $ fromIntegral ret
    where
        pid' :: CPid
        pid' = fromIntegral pid

foreign import ccall pink_bitness_name :: CInt -> CString
name :: Bitness -> IO String
name bit = peekCString $ pink_bitness_name bit'
    where
        bit' :: CInt
        bit' = fromIntegral bit
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
