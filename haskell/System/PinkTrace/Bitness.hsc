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
    Module:      System.PinkTrace.Bitness
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: non-portable, requires freebsd or linux

    Pink's bitness modes
-}
module System.PinkTrace.Bitness
    ( Bitness(..)
    , bitnessDefault
    , bitnessCountSupported
    , bitness32Supported
    , bitness64Supported
    , getBitness
    , nameBitness
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Foreign.C.Error    (throwErrno)
import Foreign.C.String   (CString, peekCString)
import Foreign.C.Types    (CInt)
import System.Posix.Types (CPid, ProcessID)
--}}}
--{{{ Foreign Imports
foreign import ccall pink_bitness_get :: CPid -> IO CInt
foreign import ccall pink_bitness_name :: CInt -> CString
--}}}
--{{{ Types
-- |Bitness modes
data Bitness = Bitness32 -- ^ 32 bit mode
    | Bitness64          -- ^ 64 bit mode
    deriving (Eq,Show)
instance Enum Bitness where
    fromEnum Bitness32 = #{const PINK_BITNESS_32}
    fromEnum Bitness64 = #{const PINK_BITNESS_64}

    toEnum #{const PINK_BITNESS_32} = Bitness32
    toEnum #{const PINK_BITNESS_64} = Bitness64
    toEnum unmatched                = error $ "Bitness.toEnum: Cannot match " ++ show unmatched
--}}}
--{{{ Functions
-- |The default bitness (eg 'Bitness32' on i386 and 'Bitness64' on x86_64)
bitnessDefault :: Bitness
bitnessDefault = toEnum #{const PINKTRACE_BITNESS_DEFAULT}

-- |Number of supported bitness (eg 1 on i386 and 2 on x86_64)
bitnessCountSupported :: Int
bitnessCountSupported = #{const PINKTRACE_BITNESS_COUNT_SUPPORTED}

-- |'True' if 'Bitness32' is a supported bitness, 'False' otherwise.
bitness32Supported :: Bool
#if PINKTRACE_BITNESS_32_SUPPORTED
bitness32Supported = True
#else
bitness32Supported = False
#endif

-- |'True' if 'Bitness64' is a supported bitness, 'False' otherwise.
bitness64Supported :: Bool
#if PINKTRACE_BITNESS_64_SUPPORTED
bitness64Supported = True
#else
bitness64Supported = False
#endif

{-|
    Return the bitness of the given process ID.

    * Note: This function calls 'throwErrno' in case of failure.
-}
getBitness :: ProcessID -> IO Bitness
getBitness pid = do
    ret <- pink_bitness_get pid
    if ret == #{const PINK_BITNESS_UNKNOWN}
        then throwErrno "pink_bitness_get"
        else return $ toEnum $ fromIntegral ret

-- |Return the string representation of the given bitness.
nameBitness :: Bitness -> IO String
nameBitness bit = peekCString $ pink_bitness_name bit'
    where
        bit' :: CInt
        bit' = fromIntegral $ fromEnum bit
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
