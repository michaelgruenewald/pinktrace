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
{-| Module:      System.PinkTrace.Event
    Copyright:   (c) Ali Polatel 2010
    License:     BSD3

    Maintainer:  alip@exherbo.org
    Stability:   provisional
    Portability: portable, ffi

    Pink's event handling
-}
--}}}
--{{{ Exports
module System.PinkTrace.Event
    ( Event(..)
    , Status
    , decide
    ) where
--}}}
--{{{ Includes
#include <pinktrace/pink.h>
--}}}
--{{{ Types
data Event = Event_Stop
    | Event_SysCall
    | Event_Fork
    | Event_VFork
    | Event_Clone
    | Event_VForkDone
    | Event_Exec
    | Event_Exit
    | Event_Genuine
    | Event_ExitGenuine
    | Event_Unknown
    deriving (Eq,Show)
type Status = Int
#ifdef PINKTRACE_LINUX
instance Enum Event where
    fromEnum Event_Stop        = #{const PINK_EVENT_STOP}
    fromEnum Event_SysCall     = #{const PINK_EVENT_SYSCALL}
    fromEnum Event_Fork        = #{const PINK_EVENT_FORK}
    fromEnum Event_VFork       = #{const PINK_EVENT_VFORK}
    fromEnum Event_Clone       = #{const PINK_EVENT_CLONE}
    fromEnum Event_VForkDone   = #{const PINK_EVENT_VFORK_DONE}
    fromEnum Event_Exec        = #{const PINK_EVENT_EXEC}
    fromEnum Event_Exit        = #{const PINK_EVENT_EXIT}
    fromEnum Event_Genuine     = #{const PINK_EVENT_GENUINE}
    fromEnum Event_ExitGenuine = #{const PINK_EVENT_EXIT_GENUINE}
    fromEnum Event_Unknown     = #{const PINK_EVENT_UNKNOWN}

    toEnum #{const PINK_EVENT_STOP}         = Event_Stop
    toEnum #{const PINK_EVENT_SYSCALL}      = Event_SysCall
    toEnum #{const PINK_EVENT_FORK}         = Event_Fork
    toEnum #{const PINK_EVENT_VFORK}        = Event_VFork
    toEnum #{const PINK_EVENT_CLONE}        = Event_Clone
    toEnum #{const PINK_EVENT_VFORK_DONE}   = Event_VForkDone
    toEnum #{const PINK_EVENT_EXEC}         = Event_Exec
    toEnum #{const PINK_EVENT_EXIT}         = Event_Exit
    toEnum #{const PINK_EVENT_GENUINE}      = Event_Genuine
    toEnum #{const PINK_EVENT_EXIT_GENUINE} = Event_ExitGenuine
    toEnum #{const PINK_EVENT_UNKNOWN}      = Event_Unknown
    toEnum unmatched                        = error $ "Event.toEnum: Cannot match " ++ show unmatched
#endif
--}}}
--{{{ Functions
#ifdef PINKTRACE_LINUX
foreign import ccall pink_event_decide :: CInt -> CInt
decide :: Status -> Event
decide st = toEnum $ fromIntegral $ pink_event_decide st'
    where
        st' :: CInt
        st' = fromIntegral st
#else
decide :: Status -> Event
decide _ = error "decide: Not implemented"
#endif
--}}}
-- vim: set ft=chaskell et ts=4 sts=4 sw=4 fdm=marker :
