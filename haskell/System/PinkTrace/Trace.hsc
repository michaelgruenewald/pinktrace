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
    ( Addr                           -- = CInt
    , EventMessage                   -- = CLong
    , TraceOption(..)
    , traceMe                        -- :: IO ()
    , traceContinue                  -- :: Addr -> Signal -> ProcessID -> IO ()
    , traceResume                    -- :: Signal -> ProcessID -> IO ()
    , traceKill                      -- :: ProcessID -> IO ()
    , traceSingleStep                -- :: Signal -> ProcessID -> IO ()
    , traceSystemCall                -- :: Signal -> ProcessID -> IO ()
    , traceSystemCallEntry           -- :: Signal -> ProcessID -> IO ()
    , traceSystemCallExit            -- :: Signal -> ProcessID -> IO ()
    , traceSystemEmulation           -- :: Signal -> ProcessID -> IO ()
    , traceSystemEmulationSingleStep -- :: Signal -> ProcessID -> IO ()
    , traceGetEventMessage           -- :: ProcessID -> IO Int
    , traceSetup                     -- :: TraceOption -> ProcessID -> IO ()
    , traceAttach                    -- :: ProcessID -> IO ()
    , traceDetach                    -- :: Signal -> ProcessID -> IO ()
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Imports
import Control.Monad        (when)
import Foreign.C.Error      (throwErrno)
import Foreign.C.Types      (CInt, CULong)
import System.Posix.Signals (Signal)
import System.Posix.Types   (CPid, ProcessID)

#ifdef PINKTRACE_LINUX
import Data.Bits             ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (peek)
#endif
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
foreign import ccall pink_trace_sysemu :: CPid -> CInt -> IO CInt
foreign import ccall pink_trace_sysemu_singlestep :: CPid -> CInt -> IO CInt
foreign import ccall pink_trace_geteventmsg :: CPid -> Ptr CULong -> IO CInt
foreign import ccall pink_trace_setup :: CPid -> CInt -> IO CInt
#endif
foreign import ccall pink_trace_attach :: CPid -> IO CInt
foreign import ccall pink_trace_detach :: CPid -> CInt -> IO CInt
--}}}
--{{{ Types
-- |Address argument of 'traceContinue' and similar functions.
type Addr = CInt
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
    when (ret == 0) (throwErrno "pink_trace_me")

{-|
    Restarts the stopped child process.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceContinue :: Addr      -- ^ On FreeBSD this argument is an address specifying the place
                           --   where execution is to be resumed (a new value for the program counter),
                           --   or 1 to indicate that execution is to pick up where it left off.
                           --   On Linux this argument is not used.
              -> Signal    -- ^ If this is non-zero and not 'sigSTOP',
                           --   it is interpreted as the signal delivered to the child;
                           --   otherwise, no signal is delivered.
              -> ProcessID -- ^ Process ID of the child to be restarted.
              -> IO ()
traceContinue addr sig pid = do
    ret <- pink_trace_cont pid sig addr
    when (ret == 0) (throwErrno "pink_trace_cont")

{-|
    Resumes the stopped child process.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceResume :: Signal    -- ^ If this is non-zero and not 'sigSTOP',
                         -- it is interpreted as the signal delivered to the child;
                         -- otherwise, no signal is delivered.
            -> ProcessID -- ^ Process ID of the child to be resumed.
            -> IO ()
traceResume = traceContinue 1

{-|
    Kills the traced child process with SIGKILL.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceKill :: ProcessID -- ^ Process ID of the child to be killed.
          -> IO ()
traceKill pid = do
    ret <- pink_trace_kill pid
    when (ret == 0) (throwErrno "pink_trace_kill")

{-|
    Restarts the stopped child process and arranges it to be stopped after
    execution of a single instruction.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceSingleStep :: Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                -> ProcessID -- ^ Process ID of the child to be restarted.
                -> IO ()
traceSingleStep sig pid = do
    ret <- pink_trace_singlestep pid sig
    when (ret == 0) (throwErrno "pink_trace_singlestep")

{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry or exit of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceSystemCall :: Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                -> ProcessID -- ^ Process ID of the child to be restarted.
                -> IO ()
traceSystemCall sig pid = do
    ret <- pink_trace_syscall pid sig
    when (ret == 0) (throwErrno "pink_trace_syscall")

{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: FreeBSD
-}
traceSystemCallEntry :: Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                     -> ProcessID -- ^ Process ID of the child to be restarted.
                     -> IO ()
#ifdef PINKTRACE_FREEBSD
traceSystemCallEntry sig pid = do
    ret <- pink_trace_syscall_entry pid sig
    when (ret == 0) (throwErrno "pink_trace_syscall_entry")
#else
traceSystemCallEntry _ _ = error "traceSystemCallEntry: not implemented"
#endif

{-|
    Restarts the stopped child process and arranges it to be stopped after the
    entry of the next system call.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: FreeBSD
-}
traceSystemCallExit :: Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                    -> ProcessID -- ^ Process ID of the child to be restarted.
                    -> IO ()
#ifdef PINKTRACE_FREEBSD
traceSystemCallExit sig pid = do
    ret <- pink_trace_syscall_exit pid sig
    when (ret == 0) (throwErrno "pink_trace_syscall_exit")
#else
traceSystemCallExit _ _ = error "traceSystemCallExit: not implemented"
#endif

{-|
    Restarts the stopped child process and arranges it to be stopped after
    the entry of the next system call which will *not* be executed.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux (2.6.14 or newer)
-}
traceSystemEmulation :: Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                     -> ProcessID -- ^ Process ID of the child to be restarted.
                     -> IO ()
#ifdef PINKTRACE_LINUX
traceSystemEmulation sig pid = do
    ret <- pink_trace_sysemu pid sig
    when (ret == 0) (throwErrno "pink_trace_sysemu")
#else
traceSystemEmulation _ _ = error "traceSystemEmulation: not implemented"
#endif

{-|
    Restarts the stopped child process like 'traceSystemEmulation' but also
    singlesteps if not a system call.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux (2.6.14 or newer)
-}
traceSystemEmulationSingleStep :: Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
                               -> ProcessID -- ^ Process ID of the child to be restarted.
                               -> IO ()
#ifdef PINKTRACE_LINUX
traceSystemEmulationSingleStep sig pid = do
    ret <- pink_trace_sysemu_singlestep pid sig
    when (ret == 0) (throwErrno "pink_trace_sysemu_singlestep")
#else
traceSystemEmulationSingleStep _ _ = error "traceSystemEmulationSingleStep: not implemented"
#endif

{-|
    Retrieve a message about the trace event that just happened.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux
-}
traceGetEventMessage :: ProcessID       -- ^ Process ID of the child whose event is to be reported.
                     -> IO EventMessage -- ^ The event message, for @Exit@ this is the child's exit status.
                                        --   For @Fork@, @VFork@, @Clone@, @VForkDone@
                                        --   this is the process ID of the new process.
#ifdef PINKTRACE_LINUX
traceGetEventMessage pid = alloca $ \ptr -> do
    ret <- pink_trace_geteventmsg pid ptr
    if ret == 0
        then throwErrno "pink_trace_geteventmsg"
        else peek ptr
#else
traceGetEventMessage _ = error "traceGetEventMessage: not implemented"
#endif

{-|
    Sets the tracing options.

    * Note: This function calls 'throwErrno' in case of failure.

    * Availability: Linux
-}
traceSetup :: TraceOption -- ^ Tracing options
           -> ProcessID   -- ^ Process ID of the child to be setup.
           -> IO ()
#if PINKTRACE_LINUX
traceSetup opt pid = do
    ret <- pink_trace_setup pid o'''''''
    when (ret == 0) (throwErrno "pink_trace_setup")
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
    when (ret == 0) (throwErrno "pink_trace_attach")

{-|
    Restarts the stopped child as for 'traceContinue', but first detaches from
    the process, undoing the reparenting effect of 'traceAttach'.

    * Note: This function calls 'throwErrno' in case of failure.
-}
traceDetach :: Signal    -- ^ Treated the same as the signal argument of 'traceContinue'.
            -> ProcessID -- ^ Process ID of the child to be detached.
            -> IO ()
traceDetach sig pid = do
    ret <- pink_trace_detach pid sig
    when (ret == 0) (throwErrno "pink_trace_detach")
--}}}
