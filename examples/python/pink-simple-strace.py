#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et tw=80 :

"""\
A simple strace like program using pinktrace
"""

from __future__ import print_function

import errno, os, signal, sys
import pinktrace.bitness
import pinktrace.event
import pinktrace.string
import pinktrace.syscall
import pinktrace.trace

def print_ret(pid):
    """Print return value"""
    ret = pinktrace.syscall.get_ret(pid)

    if (ret >= 0):
        print("= %ld" % ret, end="")
    else:
        print("= %ld %s (%s)" % (ret, errno.errorcode[-ret], os.strerror(-ret)), end="")

def decode_open(pid, bitness):
    """Decode an open() call."""

    path = pinktrace.string.decode(pid, 0, -1, bitness)
    flags = pinktrace.syscall.get_arg(pid, 1, bitness)

    print("open(\"%s\", %d)" % (path, flags) , end="")

def decode_simple(bitness, scno):
    """Decode a call simply."""

    scname = pinktrace.syscall.name(scno, bitness)

if len(sys.argv) < 2:
    print("Usage: %s program [argument...]", file=sys.stderr)
    sys.exit(1)

pid = os.fork()
if not pid: # child
    pinktrace.trace.me()
    os.kill(os.getpid(), signal.SIGSTOP)

    try:
        os.execvp(sys.argv[1], sys.argv[1:])
    except OSError:
        os._exit(-1)

os.waitpid(pid, 0)
pinktrace.trace.setup(pid, pinktrace.trace.OPTION_SYSGOOD | pinktrace.trace.OPTION_EXEC)

# parent
# Figure out the bitness of the child.
bitness = pinktrace.bitness.get(pid)
print("Child %d runs in %s mode" % (pid, pinktrace.bitness.name(bitness)))

dead = False
insyscall = False
sig = 0
exit_code = 0
while True:
    # At this point the traced child is stopped and needs to be resumed.
    pinktrace.trace.syscall(pid, sig)
    sig = 0
    pid, status = os.waitpid(pid, 0)

    # Check the event
    event = pinktrace.event.decide(status)
    if event == pinktrace.event.EVENT_SYSCALL:
        # We get this event twice, one at entering a system call and one at
        # exiting a system call.
        if insyscall:
            # Exiting the system call, print the return value
            print("", end=" ")
            print_ret(pid)
            print("")
        else:
            # Get the system call number and decode as needed.
            scno = pinktrace.syscall.get_no(pid)
            scname = pinktrace.syscall.name(scno)
            if scname is None:
                print("%ld()" % scno, end="")
            elif scname == 'open':
                decode_open(pid, bitness)
            else:
                print("%s()" % scname, end="")
        insyscall = not insyscall
    elif event == pinktrace.event.EVENT_EXEC:
        # Update bitness
        bitness = pinktrace.bitness.get(pid)
    elif event in (pinktrace.event.EVENT_GENUINE, pinktrace.event.EVENT_UNKNOWN):
        # Send the signal to the traced child as it was a genuine signal.
        sig = os.WSTOPSIG(status)
    elif event == pinktrace.event.EVENT_EXIT_GENUINE:
        exit_code = os.WEXITSTATUS(status)
        print("Child %d exited normally with return code %d" % (pid, exit_code))
        dead = True
    elif event == pinktrace.event.EVENT_EXIT_SIGNAL:
        exit_code = 128 + os.WTERMSIG(status)
        print("Child %d exited with signal %d" % (pid, os.TERMSIG(status)))
        dead = True

    if dead:
        break

sys.exit(exit_code)
