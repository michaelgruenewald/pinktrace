# vim: set sw=4 ts=4 sts=4 et :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

# Process this file with Cython.

cdef extern from "pinktrace/pink.h":
    cdef enum:
        PINK_EVENT_STOP
        PINK_EVENT_SYSCALL
        PINK_EVENT_FORK
        PINK_EVENT_VFORK
        PINK_EVENT_CLONE
        PINK_EVENT_VFORK_DONE
        PINK_EVENT_EXEC
        PINK_EVENT_EXIT
        PINK_EVENT_GENUINE
        PINK_EVENT_EXIT_GENUINE
        PINK_EVENT_EXIT_SIGNAL
        PINK_EVENT_UNKNOWN

    int pink_event_decide(int status)

EVENT_STOP = PINK_EVENT_STOP
EVENT_SYSCALL = PINK_EVENT_SYSCALL
EVENT_FORK = PINK_EVENT_FORK
EVENT_VFORK = PINK_EVENT_VFORK
EVENT_CLONE = PINK_EVENT_CLONE
EVENT_VFORK_DONE = PINK_EVENT_VFORK_DONE
EVENT_EXEC = PINK_EVENT_EXEC
EVENT_EXIT = PINK_EVENT_EXIT
EVENT_GENUINE = PINK_EVENT_GENUINE
EVENT_EXIT_GENUINE = PINK_EVENT_EXIT_GENUINE
EVENT_EXIT_SIGNAL = PINK_EVENT_EXIT_SIGNAL

class EventError(RuntimeError):
    """Raised when pinktrace.event.decide() can't decide an event."""
    pass

def decide(status):
    """
    Return the last event made by child.

    Note: This function expected pinktrace.trace.OPTION_SYSGOOD has been passed
    to pinktrace.trace.setup() or pinktrace.fork.fork().

    Note: This function raises EventError if the event is unknown.

    @param status: The status argument, received from os.waitpid() call.
    @rtype: int
    @return: One of the pinktrace.event.EVENT_* constants
    """

    cdef int event

    event = pink_event_decide(status)
    if event == PINK_EVENT_UNKNOWN:
        raise EventError("Unknown event (%#x)" % status)

    return event
