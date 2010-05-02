# vim: set sw=4 ts=4 sts=4 et :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

# Process this file with Cython.

cdef extern from "errno.h":
    int errno

cdef extern from "stdbool.h":
    int true

cdef extern from "string.h":
    char *strerror(int errnum)

cdef extern from "pinktrace/pink.h":
    cdef enum:
        PINK_TRACE_OPTION_SYSGOOD
        PINK_TRACE_OPTION_FORK
        PINK_TRACE_OPTION_VFORK
        PINK_TRACE_OPTION_CLONE
        PINK_TRACE_OPTION_EXEC
        PINK_TRACE_OPTION_VFORK_DONE
        PINK_TRACE_OPTION_EXIT
        PINK_TRACE_OPTION_ALL

    int pink_trace_me()
    int pink_trace_cont(int pid, int sig)
    int pink_trace_kill(int pid)
    int pink_trace_singlestep(int pid, int sig)
    int pink_trace_syscall(int pid, int sig)
    int pink_trace_geteventmsg(int pid, unsigned long *data)
    int pink_trace_setup(int pid, int options)
    int pink_trace_attach(int pid)
    int pink_trace_detach(int pid, int sig)

OPTION_SYSGOOD = PINK_TRACE_OPTION_SYSGOOD
OPTION_FORK = PINK_TRACE_OPTION_FORK
OPTION_VFORK = PINK_TRACE_OPTION_VFORK
OPTION_CLONE = PINK_TRACE_OPTION_CLONE
OPTION_EXEC = PINK_TRACE_OPTION_EXEC
OPTION_VFORK_DONE = PINK_TRACE_OPTION_VFORK_DONE
OPTION_EXIT = PINK_TRACE_OPTION_EXIT
OPTION_ALL = PINK_TRACE_OPTION_ALL

# Functions
def me():
    """
    Indicates that this process is to be traced by its parent.
    Any signal (except SIGKILL) delivered to this process will cause it to stop
    and its parent to be notified via wait(2). Also, all subsequent calls to
    execve(2) by this process will cause a SIGTRAP to be sent to it, giving the
    parent a chance to gain control before the new program begins execution.

    Note: This function raises OSError if the call fails.
    """

    if pink_trace_me() != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def cont(pid, sig=0):
    """
    Restarts the stopped child process.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child to be restarted
    @param sig: If this is non-zero and not SIGSTOP, it is interpreted as the
    signal to be delivered to the child; otherwise, no signal is delivered.
    Thus, for example, the parent can control whether a signal sent to the child
    is delivered or not.

    """

    if pink_trace_cont(pid, sig) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def kill(pid):
    """
    Kills the traced child process with SIGKILL.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child to be killed
    """

    if pink_trace_kill(pid) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def singlestep(pid, sig=0):
    """
    Restarts the stopped child process and arranges it to be stopped after
    execution of a single instruction.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child to be restarted
    @param sig: Treated the same as the signal argument of pinktrace.trace.cont()

    """

    if pink_trace_singlestep(pid, sig) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def syscall(pid, sig=0):
    """
    Restarts the stopped child process and arranges it to be stopped after the
    entry or exit of the next system call.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child to be restarted
    @param sig: Treated the same as the signal argument of pinktrace.trace.cont()
    """

    if pink_trace_syscall(pid, sig) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def geteventmsg(pid):
    """
    Retrieve a message (as an unsigned long) about the trace event that just
    happened, placing it in the location given by the second argument. For EXIT
    event this is the child's exit status. For FORK, VFORK, CLONE and VFORK_DONE
    events this is the process ID of the new process.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child whose event is to be reported
    @rtype: int
    @return: The message
    """

    cdef unsigned long data

    if pink_trace_geteventmsg(pid, &data) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

    return data

def setup(pid, options=OPTION_SYSGOOD):
    """
    Sets the tracing options.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child to be set up.
    @param options: Bitwise OR'ed pinktrace.trace.OPTION_* flags
    """

    if pink_trace_setup(pid, options) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def attach(pid):
    """
    Attaches to the process specified in pid, making it a traced "child" of the
    calling process; the behaviour of the child is as if it had done a
    pinktrace.trace.me(). The child is sent a SIGSTOP, but will not necessarily
    have stopped by the completion of this call; use wait(2) to wait for the
    child to stop.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child to be attached
    """

    if pink_trace_attach(pid) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def detach(pid, sig=0):
    """
    Restarts the stopped child as for pinktrace.trace.cont(), but first detaches
    from the process, undoing the reparenting effect of
    pinktrace.trace.attach().

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the child to be detached
    @param sig: Treated the same as the signal argument of pinktrace.trace.cont()
    """

    if pink_trace_detach(pid, sig) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))
