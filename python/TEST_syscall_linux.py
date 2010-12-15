#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et tw=80 :

import errno, os, signal, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, event, syscall, trace

class TestSyscallLinux_01(unittest.TestCase):

    def test_01_get_no(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.kill(os.getpid(), 0)
            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            found = False
            while ev != event.EVENT_EXIT_GENUINE:
                trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = event.decide(status)
                if ev == event.EVENT_SYSCALL:
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
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            while ev != event.EVENT_EXIT_GENUINE:
                trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = event.decide(status)
                if ev == event.EVENT_SYSCALL:
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
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            insyscall = False
            while ev != event.EVENT_EXIT_GENUINE:
                trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = event.decide(status)
                if ev == event.EVENT_SYSCALL:
                    scno = syscall.get_no(pid)
                    name = syscall.name(scno)
                    if insyscall and name == 'kill':
                        ret = syscall.get_ret(pid)
                        self.assertEqual(ret, 0)

                if not insyscall:
                    insyscall = True
                else:
                    insyscall = False

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
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD)

            # Loop until we get to the open() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            insyscall = False
            while ev != event.EVENT_EXIT_GENUINE:
                trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = event.decide(status)
                if ev == event.EVENT_SYSCALL:
                    scno = syscall.get_no(pid)
                    name = syscall.name(scno)
                    if insyscall and name == 'open':
                        ret = syscall.get_ret(pid)
                        self.assertEqual(ret, -errno.ENOENT)

                if not insyscall:
                    insyscall = True
                else:
                    insyscall = False

    def test_05_set_ret_success(self):
        # TODO: Find a smart test case for this one.
        pass

    def test_06_set_ret_fail(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            try:
                os.kill(os.getpid(), 0)
            except OSError:
                os._exit(0)
            else:
                os._exit(1)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD)

            # Loop until we get to the kill() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            insyscall = False
            while ev != event.EVENT_EXIT_GENUINE:
                trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = event.decide(status)
                if ev == event.EVENT_SYSCALL:
                    scno = syscall.get_no(pid)
                    name = syscall.name(scno)
                    if insyscall and name == 'kill':
                        syscall.set_ret(pid, -errno.EPERM)

                if not insyscall:
                    insyscall = True
                else:
                    insyscall = False

            self.assert_(os.WIFEXITED(status))
            self.assertEqual(os.WEXITSTATUS(status), 0)

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
