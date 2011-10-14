#!/usr/bin/env python
# coding: utf-8

"""
A simple strace like program using pinktrace for Linux.
"""

from __future__ import print_function

import errno, os, signal, socket, sys
import pinktrace.bitness
import pinktrace.event
import pinktrace.socket
import pinktrace.string
import pinktrace.strarray
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

def decode_execve(pid, bitness):
    """Decode an execve() call"""

    path = pinktrace.string.decode(pid, 0, -1, bitness)
    addr = pinktrace.syscall.get_arg(pid, 1, bitness)

    print("execve(\"%s\", [" % path, end="")

    i = 0
    sep = ""
    while True:
        path = pinktrace.strarray.decode(pid, addr, i)
        if path is not None:
            print("%s\"%s\"" % (sep, path), end="")
            i += 1
            sep = ", "
        else:
            print("], envp[]", end="")
            break

def decode_socketcall(pid, bitness, scname):
    """Decode a bind() or connect() call"""

    subname = None
    if pinktrace.socket.has_socketcall(bitness):
        subcall = pinktrace.socket.decode_call(pid, bitness)
        subname = pinktrace.socket.name(subcall)

        if subname not in ("bind", "connect"):
            print("%s()" % subname, end="")
            return

    addr, fd = pinktrace.socket.decode_address_fd(pid, 1, bitness)

    if subname is not None:
        print("%s(%ld, " % (subname, fd), end="")
    else:
        print("%s(%ld, " % (scname, fd), end="")

    if addr.family == -1: # NULL
        print("NULL", end="")
    elif addr.family == socket.AF_UNIX:
        if addr.abstract:
            p = "@" + addr.path
        else:
            p = addr.path
        print("{sa_family=AF_UNIX, path=%s}" % p, end="")
    elif addr.family == socket.AF_INET:
        print("{sa_family=AF_INET, sin_port=htons(%d), sin_addr=inet(\"%s\")}" % (addr.port, addr.ip), end="")
    elif pinktrace.HAVE_IPV6 and addr.family == socket.AF_INET6:
        print("{sa_family=AF_INET6, sin6_port=htons(%d), inet_pton(AF_INET6, \"%s\", &sin6_addr)}" % (addr.port, addr.ipv6), end="")
    elif pinktrace.HAVE_NETLINK and addr.family == socket.AF_NETLINK:
        print("{sa_family=AF_NETLINK, pid=%d, groups=%08x}" % (addr.pid, addr.groups), end="")
    else: # Unknown/unsupported family
        print("{sa_family=???}", end="")

    print(", %d)" % addr.length, end="")

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
            elif scname == 'execve':
                decode_execve(pid, bitness)
            elif scname in ("socketcall", "bind", "connect"):
                decode_socketcall(pid, bitness, scname)
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
