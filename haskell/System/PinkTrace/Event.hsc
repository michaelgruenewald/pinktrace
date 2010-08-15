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
    Portability: Non-portable, requires Linux

    Pink's event handling
-}
module System.PinkTrace.Event
    ( Event(..)
    , ProcessStatus(..)
    , getProcessStatus      -- :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
    , getGroupProcessStatus -- :: Bool -> Bool -> ProcessGroupID -> IO (Maybe (ProcessID, ProcessStatus))
    , getAnyProcessStatus   -- :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
#include "HsWaitPid.h"
--}}}
--{{{ Imports
import System.Exit          (ExitCode)
import System.Posix.Signals (Signal)
import System.Posix.Types   (ProcessID, ProcessGroupID)
#ifdef PINKTRACE_LINUX
import Foreign.C.Types      (CInt)
import Foreign.Ptr          (Ptr)
import System.Posix.Types   (CPid)
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
data Event = SysCall -- ^ Child has entered/exited a system call.
    | Fork           -- ^ Child has called fork().
    | VFork          -- ^ Child has called vfork().
    | Clone          -- ^ Child has called clone().
    | VForkDone      -- ^ Child has exited a vfork() call.
    | Exec           -- ^ Child has called execve().
    deriving (Eq,Ord,Show)
data ProcessStatus = Exited ExitCode -- ^ Child has exited with 'ExitCode'.
    | Terminated Signal              -- ^ Child was terminated with 'Signal'.
    | Stopped Signal                 -- ^ Child was stopped with 'Signal'.
    | StoppedTrace Event             -- ^ Child was stopped with @SIGTRAP@ due to a @ptrace@ 'Event'.

--}}}
--{{{ Local Functions
#ifdef PINKTRACE_LINUX
decipherWaitStatus :: CInt -> IO ProcessStatus
decipherWaitStatus wstat =
    case event of
        pink_EVENT_EXIT_GENUINE -> let exitstatus = c_WEXITSTATUS wstat
                                   if exitstatus == 0
                                       then return (Exited ExitSuccess)
                                       else return (ExitFailure (fromIntegral exitstatus))
        pink_EVENT_EXIT_SIGNAL  -> let termsig = c_WTERMSIG wstat
                                   return (Terminated (fromIntegral termsig))
        pink_EVENT_STOP         -> let stopsig = c_WSTOPSIG wstat
                                   return (Stopped (fromIntegral stopsig))
        pink_EVENT_GENUINE      -> let stopsig = c_WSTOPSIG wstat
                                   return (Stopped (fromIntegral stopsig))
        pink_EVENT_SYSCALL      -> return (StoppedTrace SysCall)
        pink_EVENT_FORK         -> return (StoppedTrace Fork)
        pink_EVENT_VFORK        -> return (StoppedTrace VFork)
        pink_EVENT_CLONE        -> return (StoppedTrace Clone)
        pink_EVENT_VFORK_DONE   -> return (StoppedTrace VForkDone)
        pink_EVENT_EXEC         -> return (StoppedTrace Exec)
        pink_EVENT_UNKNOWN      -> ioError (mkIOError illegalOperationErrorType "waitStatus" Nothing Nothing)
    where
        event :: Int
        event = fromIntegral $ pink_event_decide wstat
        pink_EVENT_EXIT_GENUINE :: Int
        pink_EVENT_EXIT_GENUINE = #{const PINK_EVENT_EXIT_GENUINE}
        pink_EVENT_EXIT_SIGNAL :: Int
        pink_EVENT_EXIT_SIGNAL = #{const PINK_EVENT_EXIT_SIGNAL}
        pink_EVENT_STOP :: Int
        pink_EVENT_STOP = #{const PINK_EVENT_STOP}
        pink_EVENT_GENUINE :: Int
        pink_EVENT_GENUINE = #{const PINK_EVENT_GENUINE}
        pink_EVENT_SYSCALL :: Int
        pink_EVENT_SYSCALL = #{const PINK_EVENT_SYSCALL}
        pink_EVENT_FORK :: Int
        pink_EVENT_FORK = #{const PINK_EVENT_FORK}
        pink_EVENT_VFORK :: Int
        pink_EVENT_VFORK = #{const PINK_EVENT_VFORK}
        pink_EVENT_CLONE :: Int
        pink_EVENT_CLONE = #{const PINK_EVENT_CLONE}
        pink_EVENT_VFORK_DONE :: Int
        pink_EVENT_VFORK_DONE = #{const PINK_EVENT_VFORK_DONE}
        pink_EVENT_EXEC :: Int
        pink_EVENT_EXEC = #{const PINK_EVENT_EXEC}
        pink_EVENT_UNKNOWN :: Int
        pink_EVENT_UNKNOWN = #{const PINK_EVENT_UNKNOWN}

waitOptions :: Bool -> Bool -> CInt
--             block   stopped
waitOptions False False = #{const WNOHANG}
waitOptions False True  = #{const (WNOHANG|WUNTRACED)}
waitOptions True  False = 0
waitOptions True  True  = #{const WUNTRACED}

readWaitStatus :: Ptr CInt -> IO ProcessStatus
readWaitStatus wstatp = do
    wstat <- peek wstatp
    decipherWaitStatus wstat
#endif
--}}}
--{{{ Functions
#ifdef PINKTRACE_LINUX
{-|
    @'getProcessStatus' blk stopped pid@ calls @waitpid@, returning
    @'Just' tc@, the 'ProcessStatus' for process @pid@ if it is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getProcessStatus@ of 'System.Posix.Process' module doesn't
      give information about @ptrace@ events, this is a re-implementation that
      gives information about @ptrace@ events.

    * Availability: Linux
-}
getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus block stopped pid =
    alloca $ \wstatp -> do
        pid' <- throwErrnoIfMinus1Retry "getProcessStatus"
            (c_waitpid pid wstatp (waitOptions block stopped))
        case pid' of
            0 -> return Nothing
            _ -> do ps <- readWaitStatus wstatp
                    return (Just ps)

{-|
    @'getGroupProcessStatus' blk stopped pgid@ calls @waitpid@,
    returning @'Just' (pid, tc)@, the 'ProcessID' and
    'ProcessStatus' for any process in group @pgid@ if one is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getGroupProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getGroupProcessStatus :: Bool -> Bool -> ProcessGroupID -> IO (Maybe (ProcessID, ProcessStatus))
getGroupProcessStatus block stopped pgid =
    alloca $ \wstatp -> do
    pid <- throwErrnoIfMinus1Retry "getGroupProcessStatus"
        (c_waitpid (-pgid) wstatp (waitOptions block stopped))
    case pid of
        0 -> return Nothing
        _ -> do ps <- readWaitStatus wstatp
                return (Just (pid, ps))

{-|
    @'getAnyProcessStatus' blk stopped@ calls @waitpid@, returning
    @'Just' (pid, tc)@, the 'ProcessID' and 'ProcessStatus' for any
    child process if one is available, 'Nothing' otherwise.  If
    @blk@ is 'False', then @WNOHANG@ is set in the options for
    @waitpid@, otherwise not.  If @stopped@ is 'True', then
    @WUNTRACED@ is set in the options for @waitpid@, otherwise not.

    * Note: Because @getAnyProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getAnyProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
getAnyProcessStatus block stopped = getGroupProcessStatus block stopped 1
#else
{-|
    @'getProcessStatus' blk stopped pid@ calls @waitpid@, returning
    @'Just' tc@, the 'ProcessStatus' for process @pid@ if it is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getProcessStatus@ of 'System.Posix.Process' module doesn't
      give information about @ptrace@ events, this is a re-implementation that
      gives information about @ptrace@ events.

    * Availability: Linux
-}
getProcessStatus :: Bool -> Bool -> ProcessID -> IO (Maybe ProcessStatus)
getProcessStatus _ _ _ = error "getProcessStatus: not implemented"

{-|
    @'getGroupProcessStatus' blk stopped pgid@ calls @waitpid@,
    returning @'Just' (pid, tc)@, the 'ProcessID' and
    'ProcessStatus' for any process in group @pgid@ if one is
    available, 'Nothing' otherwise.  If @blk@ is 'False', then
    @WNOHANG@ is set in the options for @waitpid@, otherwise not.
    If @stopped@ is 'True', then @WUNTRACED@ is set in the
    options for @waitpid@, otherwise not.

    * Note: Because @getGroupProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getGroupProcessStatus :: Bool -> Bool -> ProcessGroupID -> IO (Maybe (ProcessID, ProcessStatus))
getGroupProcessStatus _ _ _ = error "getGroupProcessStatus: not implemented"

{-|
    @'getAnyProcessStatus' blk stopped@ calls @waitpid@, returning
    @'Just' (pid, tc)@, the 'ProcessID' and 'ProcessStatus' for any
    child process if one is available, 'Nothing' otherwise.  If
    @blk@ is 'False', then @WNOHANG@ is set in the options for
    @waitpid@, otherwise not.  If @stopped@ is 'True', then
    @WUNTRACED@ is set in the options for @waitpid@, otherwise not.

    * Note: Because @getAnyProcessStatus@ of 'System.Posix.Process' module
      doesn't give information about @ptrace@ events, this is a
      re-implementation that gives information about @ptrace@ events.

    * Availability: Linux
-}
getAnyProcessStatus :: Bool -> Bool -> IO (Maybe (ProcessID, ProcessStatus))
getAnyProcessStatus _ _ = error "getAnyProcessStatus: not implemented"
#endif
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
