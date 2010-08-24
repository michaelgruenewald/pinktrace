{-# LANGUAGE ForeignFunctionInterface #-}

{-
 - Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 - Based in part upon System.Posix.Process module of GHC which is:
 -   (c) The University of Glasgow 2002
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
    Module:      System.PinkTrace.Event
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: non-portable, requires linux

    Pink's event handling
-}
module System.PinkTrace.Event
    ( TraceEvent(..)
    , TracedProcessStatus(..)
    , getTracedProcessStatus      -- :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
    , getGroupTracedProcessStatus -- :: Bool -> Bool -> ProcessGroupID -> IO (Maybe (ProcessID, ProcessStatus))
    , getAnyTracedProcessStatus   -- :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
#include "HsWaitPid.h"
--}}}
--{{{ Imports
import System.Exit          (ExitCode(..))
import System.Posix.Signals (Signal)
import System.Posix.Types   (ProcessID, ProcessGroupID)
#ifdef PINKTRACE_LINUX
import Foreign.C.Error       (throwErrnoIfMinus1Retry)
import Foreign.C.Types       (CInt)
import Foreign.Ptr           (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable      (peek)
import System.Posix.Types    (CPid)
import System.IO.Error       (illegalOperationErrorType, mkIOError)
#endif
--}}}
--{{{ Foreign Imports
#ifdef PINKTRACE_LINUX
-- safe, because this call might block
foreign import ccall safe "waitpid" c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
foreign import ccall unsafe "__pinkhs_wexitstatus" c_WEXITSTATUS :: CInt -> CInt
foreign import ccall unsafe "__pinkhs_wtermsig"    c_WTERMSIG :: CInt -> CInt
foreign import ccall unsafe "__pinkhs_wstopsig"    c_WSTOPSIG :: CInt -> CInt

foreign import ccall pink_event_decide :: CInt -> CInt
#endif
--}}}
--{{{ Types
data TraceEvent = SysCall -- ^ Child has entered/exited a system call.
    | Fork                -- ^ Child has called fork().
    | VFork               -- ^ Child has called vfork().
    | Clone               -- ^ Child has called clone().
    | VForkDone           -- ^ Child has exited a vfork() call.
    | Exec                -- ^ Child has called execve().
    | Exit                -- ^ Child is exiting. (@ptrace@ way, stopped before exit)
    deriving (Eq,Ord,Show)
data TracedProcessStatus = Exited ExitCode -- ^ Child has exited with 'ExitCode'.
    | Terminated Signal                    -- ^ Child was terminated with 'Signal'.
    | Stopped Signal                       -- ^ Child was stopped with 'Signal'.
    | StoppedTrace TraceEvent              -- ^ Child was stopped with @SIGTRAP@ due to a @ptrace@ 'TraceEvent'.

--}}}
--{{{ Local Functions
#ifdef PINKTRACE_LINUX
decipherWaitStatus :: CInt -> IO TracedProcessStatus
decipherWaitStatus wstat =
    if event == #{const PINK_EVENT_EXIT_GENUINE}
        then do
             let exitstatus = c_WEXITSTATUS wstat
             if exitstatus == 0
                 then return (Exited ExitSuccess)
                 else return (Exited (ExitFailure (fromIntegral exitstatus)))
    else
        if event == #{const PINK_EVENT_EXIT_SIGNAL}
            then do
                  let termsig = c_WTERMSIG wstat
                  return (Terminated (fromIntegral termsig))
            else
                if event == #{const PINK_EVENT_STOP} || event == #{const PINK_EVENT_GENUINE}
                    then do
                         let stopsig = c_WSTOPSIG wstat
                         return (Stopped (fromIntegral stopsig))
                    else
                        if event == #{const PINK_EVENT_SYSCALL}
                            then return (StoppedTrace SysCall)
                            else
                                if event == #{const PINK_EVENT_FORK}
                                    then return (StoppedTrace Fork)
                                    else
                                        if event == #{const PINK_EVENT_VFORK}
                                            then return (StoppedTrace VFork)
                                            else
                                                if event == #{const PINK_EVENT_CLONE}
                                                    then return (StoppedTrace Clone)
                                                    else
                                                        if event == #{const PINK_EVENT_VFORK_DONE}
                                                            then return (StoppedTrace VForkDone)
                                                            else
                                                                if event == #{const PINK_EVENT_EXEC}
                                                                    then return (StoppedTrace Exec)
                                                                    else
                                                                        if event == #{const PINK_EVENT_EXIT}
                                                                            then return (StoppedTrace Exit)
                                                                            else ioError (mkIOError
                                                                                          illegalOperationErrorType
                                                                                          "waitStatus"
                                                                                          Nothing Nothing)
    where
        event :: Int
        event = fromIntegral $ pink_event_decide wstat

waitOptions :: Bool -> Bool -> CInt
--             block   stopped
waitOptions False False = #{const WNOHANG}
waitOptions False True  = #{const (WNOHANG|WUNTRACED)}
waitOptions True  False = 0
waitOptions True  True  = #{const WUNTRACED}

readWaitStatus :: Ptr CInt -> IO TracedProcessStatus
readWaitStatus wstatp = do
    wstat <- peek wstatp
    decipherWaitStatus wstat
#endif
--}}}
--{{{ Functions
#ifdef PINKTRACE_LINUX
{-|
    @'getTracedProcessStatus' blk stopped pid@ calls @waitpid@, returning
    @'Just' tc@, the 'TracedProcessStatus' for process @pid@ if it is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getProcessStatus@ of 'System.Posix.Process' module doesn't
      give information about @ptrace@ events, this is a re-implementation that
      gives information about @ptrace@ events.

    * Availability: Linux
-}
getTracedProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe TracedProcessStatus)
getTracedProcessStatus block stopped pid =
    alloca $ \wstatp -> do
        pid' <- throwErrnoIfMinus1Retry "getTracedProcessStatus"
            (c_waitpid pid wstatp (waitOptions block stopped))
        case pid' of
            0 -> return Nothing
            _ -> do ps <- readWaitStatus wstatp
                    return (Just ps)

{-|
    @'getGroupTracedProcessStatus' blk stopped pgid@ calls @waitpid@,
    returning @'Just' (pid, tc)@, the 'ProcessID' and
    'TracedProcessStatus' for any process in group @pgid@ if one is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getGroupProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getGroupTracedProcessStatus :: Bool -> Bool -> ProcessGroupID -> IO (Maybe (ProcessID, TracedProcessStatus))
getGroupTracedProcessStatus block stopped pgid =
    alloca $ \wstatp -> do
    pid <- throwErrnoIfMinus1Retry "getGroupTracedProcessStatus"
        (c_waitpid (-pgid) wstatp (waitOptions block stopped))
    case pid of
        0 -> return Nothing
        _ -> do ps <- readWaitStatus wstatp
                return (Just (pid, ps))

{-|
    @'getAnyTracedProcessStatus' blk stopped@ calls @waitpid@, returning
    @'Just' (pid, tc)@, the 'ProcessID' and 'TracedProcessStatus' for any
    child process if one is available, 'Nothing' otherwise.  If
    @blk@ is 'False', then @WNOHANG@ is set in the options for
    @waitpid@, otherwise not.  If @stopped@ is 'True', then
    @WUNTRACED@ is set in the options for @waitpid@, otherwise not.

    * Note: Because @getAnyProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getAnyTracedProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, TracedProcessStatus))
getAnyTracedProcessStatus block stopped = getGroupProcessStatus block stopped 1
#else
{-|
    @'getTracedProcessStatus' blk stopped pid@ calls @waitpid@, returning
    @'Just' tc@, the 'TracedProcessStatus' for process @pid@ if it is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getProcessStatus@ of 'System.Posix.Process' module doesn't
      give information about @ptrace@ events, this is a re-implementation that
      gives information about @ptrace@ events.

    * Availability: Linux
-}
getTracedProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe TracedProcessStatus)
getTracedProcessStatus _ _ _ = error "getTracedProcessStatus: not implemented"

{-|
    @'getGroupTracedProcessStatus' blk stopped pgid@ calls @waitpid@,
    returning @'Just' (pid, tc)@, the 'ProcessID' and
    'TracedProcessStatus' for any process in group @pgid@ if one is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getGroupProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getGroupTracedProcessStatus :: Bool -> Bool -> ProcessGroupID -> IO (Maybe (ProcessID, TracedProcessStatus))
getGroupTracedProcessStatus _ _ _ = error "getGroupTracedProcessStatus: not implemented"

{-|
    @'getAnyTracedProcessStatus' blk stopped@ calls @waitpid@, returning
    @'Just' (pid, tc)@, the 'ProcessID' and 'TracedProcessStatus' for any
    child process if one is available, 'Nothing' otherwise.  If
    @blk@ is 'False', then @WNOHANG@ is set in the options for
    @waitpid@, otherwise not.  If @stopped@ is 'True', then
    @WUNTRACED@ is set in the options for @waitpid@, otherwise not.

    * Note: Because @getAnyProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getAnyTracedProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, TracedProcessStatus))
getAnyTracedProcessStatus _ _ = error "getAnyTracedProcessStatus: not implemented"
#endif
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
