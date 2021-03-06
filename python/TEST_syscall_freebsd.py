#!/usr/bin/env python
# coding: utf-8

import errno, os, signal, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, event, syscall, trace

class TestSyscallFreeBSD_01(unittest.TestCase):

    def test_01_get_no(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.kill(os.getpid(), 0)
            os._exit(0)
        else: # parent
            pid, status = os.waitpid(pid, 0)
            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            while True:
                trace.syscall_entry(pid, 0)
                pid, status = os.waitpid(pid, 0)
                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)

                scno = syscall.get_no(pid)
                name = syscall.name(scno)
                if name == 'kill':
                    found = True
                    break

            self.assert_(found)

            try: trace.kill(pid)
            except OSError: pass

    def test_02_set_no(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.kill(os.getpid(), 0)
            os._exit(0)
        else: # parent
            pid, status = os.waitpid(pid, 0)
            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            while True:
                trace.syscall_entry(pid, 0)
                pid, status = os.waitpid(pid, 0)
                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)

                scno = syscall.get_no(pid)
                name = syscall.name(scno)
                if name == 'kill':
                    syscall.set_no(pid, syscall.INVALID)
                    scno = syscall.get_no(pid)
                    self.assertEqual(scno, syscall.INVALID)
                    break

            try: trace.kill(pid)
            except OSError: pass

    def test_03_get_ret_success(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.kill(os.getpid(), 0)
            os._exit(0)
        else: # parent
            pid, status = os.waitpid(pid, 0)
            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            stop_at_exit = False
            while True:
                if stop_at_exit:
                    trace.syscall_exit(pid, 0)
                else:
                    trace.syscall_entry(pid, 0)
                pid, status = os.waitpid(pid, 0)
                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)

                scno = syscall.get_no(pid)
                name = syscall.name(scno)
                if name == 'kill':
                    stop_at_exit = True
                    continue
                elif stop_at_exit:
                    ret = syscall.get_ret(pid)
                    self.assertEqual(ret, 0)
                    break

            try: trace.kill(pid)
            except OSError: pass


    def test_04_get_ret_fail(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            try:
                open('')
            except IOError:
                pass
            os._exit(0)
        else: # parent
            pid, status = os.waitpid(pid, 0)
            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)

            # Loop until we get to the open() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            stop_at_exit = False
            while True:
                if stop_at_exit:
                    trace.syscall_exit(pid, 0)
                else:
                    trace.syscall_entry(pid, 0)
                pid, status = os.waitpid(pid, 0)
                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)

                scno = syscall.get_no(pid)
                name = syscall.name(scno)
                if name == 'open':
                    stop_at_exit = True
                    continue
                elif stop_at_exit:
                    ret = syscall.get_ret(pid)
                    self.assertEqual(ret, -errno.ENOENT)
                    break

            try: trace.kill(pid)
            except OSError: pass


    def test_05_set_ret_success(self):
        # TODO: Find a smart test case for this one.
        pass

    def test_06_set_ret_fail(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.kill(os.getpid(), 0)
        else: # parent
            pid, status = os.waitpid(pid, 0)
            self.assert_(os.WIFSTOPPED(status), "%#x" % status)
            self.assertEqual(os.WSTOPSIG(status), signal.SIGSTOP, "%#x" % status)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            stop_at_exit = False
            while True:
                if stop_at_exit:
                    trace.syscall_exit(pid, 0)
                else:
                    trace.syscall_entry(pid, 0)
                pid, status = os.waitpid(pid, 0)
                self.assert_(os.WIFSTOPPED(status), "%#x" % status)
                self.assertEqual(os.WSTOPSIG(status), signal.SIGTRAP, "%#x" %  status)

                scno = syscall.get_no(pid)
                name = syscall.name(scno)
                if name == 'kill':
                    stop_at_exit = True
                    continue
                elif stop_at_exit:
                    syscall.set_ret(pid, -errno.ENOENT)
                    ret = syscall.get_ret(pid)
                    self.assertEqual(ret, -errno.ENOENT)
                    break

            try: trace.kill(pid)
            except OSError: pass

    def test_07_get_arg_first(self):
        pass

    def test_08_get_arg_second(self):
        pass

    def test_09_get_arg_third(self):
        pass

    def test_10_get_arg_fourth(self):
        pass

    def test_11_get_arg_fifth(self):
        pass

    def test_12_get_arg_sixth(self):
        pass

if __name__ == '__main__':
    unittest.main()
