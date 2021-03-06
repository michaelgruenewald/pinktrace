#!/usr/bin/env python
# coding: utf-8

import os, signal, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, syscall, strarray, trace

class TestStringArrayLinux_02(unittest.TestCase):

    def test_01_decode(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.execvp("true", ("/dev/null",))
            os._exit(0)
        else: # parent
            pid, status = os.waitpid(pid, 0)
            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)

            # Loop until we get to the open() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            while True:
                trace.syscall_entry(pid, 0)
                pid, status = os.waitpid(pid, 0)
                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)

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
            pid, status = os.waitpid(pid, 0)
            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)

            # Loop until we get to the open() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            while True:
                trace.syscall_entry(pid, 0)
                pid, status = os.waitpid(pid, 0)
                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)

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
