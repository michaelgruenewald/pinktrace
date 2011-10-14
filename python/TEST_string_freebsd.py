#!/usr/bin/env python
# coding: utf-8

import os, signal, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, syscall, string, trace

class TestStringLinux_02(unittest.TestCase):

    def test_01_decode(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            open('/dev/null', 'r')
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
                if name == 'open':
                    path = string.decode(pid, 0)
                    self.assertEqual(path, '/dev/null')
                    break

            try: trace.kill(pid)
            except OSError: pass

    def test_02_decode_max(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            open('/dev/null', 'r')
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
                if name == 'open':
                    path = string.decode(pid, 0, 9)
                    self.assertEqual(path, '/dev/null')
                    break

            try: trace.kill(pid)
            except OSError: pass

     # FIXME: This test hangs on FreeBSD/amd64
#    def test_03_encode(self):
#        pid = os.fork()
#        if not pid: # child
#            trace.me()
#            os.kill(os.getpid(), signal.SIGSTOP)
#
#            open('/dev/null', 'r')
#        else: # parent
#            pid, status = os.waitpid(pid, 0)
#            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
#            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)
#
#            # Loop until we get to the open() system call as there's no
#            # guarantee that other system calls won't be called beforehand.
#            while True:
#                trace.syscall_entry(pid, 0)
#                pid, status = os.waitpid(pid, 0)
#                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
#                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)
#
#                scno = syscall.get_no(pid)
#                name = syscall.name(scno)
#                if name == 'open':
#                    string.encode(pid, 0, '/dev/null')
#                    path = string.decode(pid, 0)
#                    self.assertEqual(path, '/dev/null')
#                    break
#
#            try: trace.kill(pid)
#            except OSError: pass

if __name__ == '__main__':
    unittest.main()
