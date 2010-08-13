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
{-| Module:      System.PinkTrace.Trace
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: portable, ffi

    Pink's low level wrappers around ptrace(2) internals
-}
--}}}
--{{{ Exports
module System.PinkTrace.Trace
    ( Option(..)
    , me
    , cont
    , kill
    , singlestep
    , syscall
    , syscall_entry
    , syscall_exit
    , geteventmsg
    , setup
    , attach
    , detach
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Foreign.C.Error    (throwErrno)
import System.Posix.Types (CPid)
#ifdef PINKTRACE_LINUX
import Data.Bits          ((.|.))
import Foreign.Ptr        (Ptr)
#endif
import Foreign.C.Types    ( CInt
#ifdef PINKTRACE_LINUX
                          , CULong
#endif
                          )

import System.PinkTrace   (Addr, Pid, Sig)
--}}}
--{{{ Types
data Option = Option
    { sysGood   :: Bool
    , fork      :: Bool
    , vFork     :: Bool
    , clone     :: Bool
    , exec      :: Bool
    , vForkDone :: Bool
    , exit      :: Bool
    } deriving (Show)
--}}}
--{{{ Functions
foreign import ccall pink_trace_me :: IO CInt
me :: IO ()
me = do
    ret <- pink_trace_me
    if ret == 0
        then throwErrno "pink_trace_me"
        else return ()

foreign import ccall pink_trace_cont :: CPid -> CInt -> CInt -> IO CInt
cont :: Pid -> Sig -> Addr -> IO ()
cont pid sig addr = do
    ret <- pink_trace_cont pid' sig' addr'
    if ret == 0
        then throwErrno "pink_trace_cont"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        sig' :: CInt
        sig' = fromIntegral sig
        addr' :: CInt
        addr' = fromIntegral addr

foreign import ccall pink_trace_kill :: CPid -> IO CInt
kill :: Pid -> IO ()
kill pid = do
    ret <- pink_trace_kill pid'
    if ret == 0
        then throwErrno "pink_trace_kill"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid

foreign import ccall pink_trace_singlestep :: CPid -> CInt -> IO CInt
singlestep :: Pid -> Sig -> IO ()
singlestep pid sig = do
    ret <- pink_trace_singlestep pid' sig'
    if ret == 0
        then throwErrno "pink_trace_singlestep"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        sig' :: CInt
        sig' = fromIntegral sig

foreign import ccall pink_trace_syscall :: CPid -> CInt -> IO CInt
syscall :: Pid -> Sig -> IO ()
syscall pid sig = do
    ret <- pink_trace_syscall pid' sig'
    if ret == 0
        then throwErrno "pink_trace_syscall"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        sig' :: CInt
        sig' = fromIntegral sig

#ifdef PINKTRACE_FREEBSD
foreign import ccall pink_trace_syscall_entry :: CPid -> CInt -> IO CInt
syscall_entry :: Pid -> Sig -> IO ()
syscall_entry pid sig = do
    ret <- pink_trace_syscall_entry pid' sig'
    if ret == 0
        then throwErrno "pink_trace_syscall_entry"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        sig' :: CInt
        sig' = fromIntegral sig

foreign import ccall pink_trace_syscall_exit :: CPid -> CInt -> IO CInt
syscall_exit :: Pid -> Sig -> IO ()
syscall_exit pid sig = do
    ret <- pink_trace_syscall_exit pid' sig'
    if ret == 0
        then throwErrno "pink_trace_syscall_exit"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        sig' :: CInt
        sig' = fromIntegral sig
#else
syscall_entry :: Pid -> Sig -> IO ()
syscall_entry _ _ = error "syscall_entry: not implemented"

syscall_exit :: Pid -> Sig -> IO ()
syscall_exit _ _ = error "syscall_exit: not implemented"
#endif

#ifdef PINKTRACE_LINUX
foreign import ccall pink_trace_geteventmsg :: CPid -> Ptr CULong -> IO CInt
geteventmsg :: Pid -> IO Int
geteventmsg pid = alloca $ \ptr -> do
    ret <- pink_trace_geteventmsg pid' ptr
    if ret == 0
        then throwErrno "pink_trace_geteventmsg"
        else return $ fromIntegral $ peek ptr
    where
        pid' :: CPid
        pid' = fromIntegral pid

foreign import ccall pink_trace_setup :: CPid -> CInt -> IO CInt
setup :: Pid -> Option -> IO ()
setup pid opt = do
    ret <- #{call pink_trace_setup} pid' o'''''''
    if ret == 0
        then throwErrno "pink_trace_setup"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        o :: CInt
        o        = 0
        o'       = if sysGood opt   then o       .|. #{const PINK_TRACE_OPTION_SYSGOOD}    else o
        o''      = if fork opt      then o'      .|. #{const PINK_TRACE_OPTION_FORK}       else o'
        o'''     = if vFork opt     then o''     .|. #{const PINK_TRACE_OPTION_VFORK}      else o''
        o''''    = if clone opt     then o'''    .|. #{const PINK_TRACE_OPTION_CLONE}      else o'''
        o'''''   = if exec opt      then o''''   .|. #{const PINK_TRACE_OPTION_EXEC}       else o''''
        o''''''  = if vForkDone opt then o'''''  .|. #{const PINK_TRACE_OPTION_VFORK_DONE} else o'''''
        o''''''' = if exit opt      then o'''''' .|. #{const PINK_TRACE_OPTION_EXIT}       else o''''''
#else
geteventmsg :: Pid -> IO Int
geteventmsg _ = error "geteventmsg: not implemented"

setup :: Pid -> Option -> IO ()
setup _ _ = error "setup: not implemented"
#endif

foreign import ccall pink_trace_attach :: CPid -> IO CInt
attach :: Pid -> IO ()
attach pid = do
    ret <- pink_trace_attach pid'
    if ret == 0
        then throwErrno "pink_trace_attach"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid

foreign import ccall pink_trace_detach :: CPid -> CInt -> IO CInt
detach :: Pid -> Sig -> IO ()
detach pid sig = do
    ret <- pink_trace_detach pid' sig'
    if ret == 0
        then throwErrno "pink_trace_detach"
        else return ()
    where
        pid' :: CPid
        pid' = fromIntegral pid
        sig' :: CInt
        sig' = fromIntegral sig
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
