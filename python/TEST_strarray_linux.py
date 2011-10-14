#!/usr/bin/env python
# coding: utf-8

import os, signal, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, event, syscall, strarray, trace

class TestStringLinux_02(unittest.TestCase):

    def test_01_decode(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.execvp("true", ("/dev/null",))
            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD)

            # Loop until we get to the open() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            while ev != event.EVENT_EXIT_GENUINE:
                trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = event.decide(status)
                if ev == event.EVENT_SYSCALL:
                    scno = syscall.get_no(pid)
                    name = syscall.name(scno)
                    if name == 'execve':
                        arg = syscall.get_arg(pid, 1)
                        path = strarray.decode(pid, arg, 0)
                        self.assertEqual(path, '/dev/null')
                        break

            try: trace.kill(pid)
            except OSError: pass

    def test_02_decode_max(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.execvp('true', ('/dev/null',))
            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD)

            # Loop until we get to the open() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            while ev != event.EVENT_EXIT_GENUINE:
                trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = event.decide(status)
                if ev == event.EVENT_SYSCALL:
                    scno = syscall.get_no(pid)
                    name = syscall.name(scno)
                    if name == 'execve':
                        arg = syscall.get_arg(pid, 1)
                        path = strarray.decode(pid, arg, 0, 9)
                        self.assertEqual(path, '/dev/null')
                        break

            try: trace.kill(pid)
            except OSError: pass

if __name__ == '__main__':
    unittest.main()
