#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set sw=4 ts=4 sts=4 et tw=80 :

import os, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, event, fork, syscall, string, trace

class TestString_01_Invalid(unittest.TestCase):

    def test_01_decode(self):
        self.assertRaises(TypeError, string.decode)
        self.assertRaises(TypeError, string.decode, 0)
        self.assertRaises(IndexError, string.decode, 0, syscall.MAX_INDEX)
        self.assertRaises(bitness.BitnessError, string.decode, 0, 1, -1, 13)

class TestString_02(unittest.TestCase):

    def test_01_decode(self):
        pid = fork.fork()
        if pid == 0: # child
            open('/dev/null', 'r')
            os._exit(0)
        else: # parent
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
                    if name == 'open':
                        path = string.decode(pid, 0)
                        self.assertEqual(path, '/dev/null')
                        break

            try: trace.kill(pid)
            except OSError: pass

    def test_02_decode_max(self):
        pid = fork.fork()
        if pid == 0: # child
            open('/dev/null', 'r')
            os._exit(0)
        else: # parent
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
                    if name == 'open':
                        path = string.decode(pid, 0, 9)
                        self.assertEqual(path, '/dev/null')
                        break

            try: trace.kill(pid)
            except OSError: pass

    def test_03_encode(self):
        pid = fork.fork()
        if pid == 0: # child
            try:
                open('/dev/null', 'r')
            except IOError:
                os._exit(0)
            else:
                os._exit(1)
        else: # parent
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
                    if name == 'open':
                        string.encode(pid, 0, '/dev/NULL')

            self.assert_(os.WIFEXITED(status))
            self.assertEqual(os.WEXITSTATUS(status), 0)

if __name__ == '__main__':
    unittest.main()
