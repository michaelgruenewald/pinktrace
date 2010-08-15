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
    Module:      System.PinkTrace.Trace
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: non-portable, requires freebsd or linux

    Pink's low level wrappers around ptrace(2) internals
-}
module System.PinkTrace.Trace
    ( EventMessage          -- = CLong
    , TraceOption(..)
    , traceMe               -- :: IO ()
    , traceContinue         -- :: ProcessID -> Signal -> Addr -> IO ()
    , traceKill             -- :: ProcessID -> IO ()
    , traceSingleStep       -- :: ProcessID -> Signal -> IO ()
    , traceSysCall          -- :: ProcessID -> Signal -> IO ()
    , traceSysCallEntry     -- :: ProcessID -> Signal -> IO ()
    , traceSysCallExit      -- :: ProcessID -> Signal -> IO ()
    , traceGetEventMessage  -- :: ProcessID -> IO Int
    , traceSetup            -- :: ProcessID -> TraceOption -> IO ()
    , traceAttach           -- :: ProcessID -> IO ()
    , traceDetach           -- :: ProcessID -> Signal -> IO ()
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Foreign.C.Error      (throwErrno)
import System.Posix.Signals (Signal)
import System.Posix.Types   (CPid, ProcessID)
#ifdef PINKTRACE_LINUX
import Data.Bits            ((.|.))
import Foreign.Ptr          (Ptr)
#endif
import Foreign.C.Types      (CInt, CULong)

import System.PinkTrace     (Addr)
--}}}
--{{{ Foreign Imports
foreign import ccall pink_trace_me :: IO CInt
foreign import ccall pink_trace_cont :: CPid -> CInt -> CInt -> IO CInt
foreign import ccall pink_trace_kill :: CPid -> IO CInt
foreign import ccall pink_trace_singlestep :: CPid -> CInt -> IO CInt
foreign import ccall pink_trace_syscall :: CPid -> CInt -> IO CInt
#ifdef PINKTRACE_FREEBSD
foreign import ccall pink_trace_syscall_entry :: CPid -> CInt -> IO CInt
foreign import ccall pink_trace_syscall_exit :: CPid -> CInt -> IO CInt
#endif
#ifdef PINKTRACE_LINUX
foreign import ccall pink_trace_geteventmsg :: CPid -> Ptr CULong -> IO CInt
foreign import ccall pink_trace_setup :: CPid -> CInt -> IO CInt
#endif
foreign import ccall pink_trace_attach :: CPid -> IO CInt
foreign import ccall pink_trace_detach :: CPid -> CInt -> IO CInt
--}}}
--{{{ Types
-- |EventMessage returned by 'traceGetEventMessage'.
type EventMessage = CULong
-- |Trace options that may be passed to 'traceSetup'.
data TraceOption = TraceOption
    { traceOptionSysGood   :: Bool -- ^ When delivering system call traps, set bit 7 in signal number.
    , traceOptionFork      :: Bool -- ^ Stop the child at the next fork(2) call.
    , traceOptionVFork     :: Bool -- ^ Stop the child at the next vfork(2) call.
    , traceOptionClone     :: Bool -- ^ Stop the child at the next clone(2) call.
    , traceOptionExec      :: Bool -- ^ Stop the child at the next execve(2) call.
    , traceOptionVForkDone :: Bool -- ^ Stop the child at the completion of the next vfork(2) call.
    , traceOptionExit      :: Bool -- ^ Stop the child at exit.
    } deriving (Show)
--}}}
--{{{ Functions
{-|
    Indicates that this process is to be traced by its parent. Any signal
    (except SIGKILL) delivered to this process will cause it to stop and its
    parent to be notified via wait(2). Also, all subsequent calls to execve(2)
    by this process will cause a SIGTRAP to be sent to it, giving the parent a
    chance to gain control before the new program begins execution.

    * Note: This function is used only by the child process; the rest are used
    only by the parent.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceMe :: IO ()
traceMe = do
    ret <- pink_trace_me
    if ret == 0
        then throwErrno "pink_trace_me"
        else return ()

{-|
    Restarts the stopped child process.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceContinue :: ProcessID -- ^ Process ID of the child to be restarted.
              -> Signal    -- ^ If this is non-zero and not 'sigSTOP',
                           --   it is interpreted as the signal delivered to the child;
                           --   otherwise, no signal is delivered.
              -> Addr      -- ^ On FreeBSD this argument is an address specifying the place
                           --   where execution is to be resumed (a new value for the program counter),
                           --   or 1 to indicate that execution is to pick up where it left off.
                           --   On Linux this argument is not used.
              -> IO ()
traceContinue pid sig addr = do
    ret <- pink_trace_cont pid sig addr'
    if ret == 0
        then throwErrno "pink_trace_cont"
        else return ()
    where
        addr' :: CInt
        addr' = fromIntegral addr

{-|
    Kills the traced child process with SIGKILL.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceKill :: ProcessID -- ^ Process ID of the child to be killed.
          -> IO ()
traceKill pid = do
    ret <- pink_trace_kill pid
    if ret == 0
        then throwErrno "pink_trace_kill"
        else return ()

{-|
    Restarts the stopped child process and arranges it to be stopped after
    execution of a single instruction.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceSingleStep :: ProcessID -- ^ Process ID of the child to be restarted.
                -> Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                -> IO ()
traceSingleStep pid sig = do
    ret <- pink_trace_singlestep pid sig
    if ret == 0
        then throwErrno "pink_trace_singlestep"
        else return ()

{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry or exit of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceSysCall :: ProcessID -- ^ Process ID of the child to be restarted.
             -> Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
             -> IO ()
traceSysCall pid sig = do
    ret <- pink_trace_syscall pid sig
    if ret == 0
        then throwErrno "pink_trace_syscall"
        else return ()

#ifdef PINKTRACE_FREEBSD
{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: FreeBSD
-}
traceSysCallEntry :: ProcessID -- ^ Process ID of the child to be restarted.
                  -> Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                  -> IO ()
traceSysCallEntry pid sig = do
    ret <- pink_trace_syscall_entry pid sig
    if ret == 0
        then throwErrno "pink_trace_syscall_entry"
        else return ()

{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: FreeBSD
-}
traceSysCallExit :: ProcessID -- ^ Process ID of the child to be restarted.
                 -> Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                 -> IO ()
traceSysCallExit pid sig = do
    ret <- pink_trace_syscall_exit pid sig
    if ret == 0
        then throwErrno "pink_trace_syscall_exit"
        else return ()
#else
{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: FreeBSD
-}
traceSysCallEntry :: ProcessID -- ^ Process ID of the child to be restarted.
                  -> Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                  -> IO ()
traceSysCallEntry _ _ = error "syscallEntry: not implemented"

{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: FreeBSD
-}
traceSysCallExit :: ProcessID -- ^ Process ID of the child to be restarted.
                 -> Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                 -> IO ()
traceSysCallExit _ _ = error "syscallExit: not implemented"
#endif

#ifdef PINKTRACE_LINUX
{-|
    Retrieve a message about the trace event that just happened.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux
-}
traceGetEventMessage :: ProcessID       -- ^ Process ID of the child whose event is to be reported.
                     -> IO EventMessage -- ^ The event message, for 'Event_Exit' this is the child's exit status.
                                        --   For 'Event_Fork', 'Event_VFork', 'Event_Clone', 'Event_VForkDone'
                                        --   this is the process ID of the new process.
traceGetEventMessage pid = alloca $ \ptr -> do
    ret <- pink_trace_geteventmsg pid ptr
    if ret == 0
        then throwErrno "pink_trace_geteventmsg"
        else return $ peek ptr

{-|
    Sets the tracing options.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux
-}
traceSetup :: ProcessID   -- ^ Process ID of the child to be setup.
           -> TraceOption -- ^ Tracing options
           -> IO ()
traceSetup pid opt = do
    ret <- pink_trace_setup pid o'''''''
    if ret == 0
        then throwErrno "pink_trace_setup"
        else return ()
    where
        o :: CInt
        o        = 0
        o'       = if traceOptionSysGood opt   then o       .|. #{const PINK_TRACE_OPTION_SYSGOOD}    else o
        o''      = if traceOptionFork opt      then o'      .|. #{const PINK_TRACE_OPTION_FORK}       else o'
        o'''     = if traceOptionVFork opt     then o''     .|. #{const PINK_TRACE_OPTION_VFORK}      else o''
        o''''    = if traceOptionClone opt     then o'''    .|. #{const PINK_TRACE_OPTION_CLONE}      else o'''
        o'''''   = if traceOptionExec opt      then o''''   .|. #{const PINK_TRACE_OPTION_EXEC}       else o''''
        o''''''  = if traceOptionVForkDone opt then o'''''  .|. #{const PINK_TRACE_OPTION_VFORK_DONE} else o'''''
        o''''''' = if traceOptionExit opt      then o'''''' .|. #{const PINK_TRACE_OPTION_EXIT}       else o''''''
#else
{-|
    Retrieve a message about the trace event that just happened.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux
-}
traceGetEventMessage :: ProcessID       -- ^ Process ID of the child whose event is to be reported.
                     -> IO EventMessage -- ^ The event message, for 'Event_Exit' this is the child's exit status.
                                        --   For 'Event_Fork', 'Event_VFork', 'Event_Clone', 'Event_VForkDone'
                                        --   this is the process ID of the new process.
traceGetEventMessage _ = error "traceGetEventMessage: not implemented"

{-|
    Sets the tracing options.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux
-}
traceSetup :: ProcessID   -- ^ Process ID of the child to be setup.
           -> TraceOption -- ^ Tracing options
           -> IO ()
traceSetup _ _ = error "traceSetup: not implemented"
#endif

{-|
    Attaches to the process specified in pid, making it a traced child of the
    calling process; the behaviour of the child is as if it had done a
    'traceMe'. The child is sent a 'sigSTOP', but will not necessarily have
    stopped by the completion of this call; use wait(2) to wait for the child
    to stop.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceAttach :: ProcessID -- ^ Process ID of the child to be attached.
            -> IO ()
traceAttach pid = do
    ret <- pink_trace_attach pid
    if ret == 0
        then throwErrno "pink_trace_attach"
        else return ()

{-|
    Restarts the stopped child as for 'traceContinue', but first detaches from
    the process, undoing the reparenting effect of 'traceAttach'.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceDetach :: ProcessID -- ^ Process ID of the child to be detached.
            -> Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
            -> IO ()
traceDetach pid sig = do
    ret <- pink_trace_detach pid sig
    if ret == 0
        then throwErrno "pink_trace_detach"
        else return ()
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
