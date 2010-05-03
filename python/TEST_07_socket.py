#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set sw=4 ts=4 sts=4 et :

import os, socket, sys, unittest

sys.path.insert(0, '.')
import pinktrace

class TestSocket_01_Invalid(unittest.TestCase):

    def test_01_name(self):
        self.assertRaises(TypeError, pinktrace.socket.name)

    def test_02_decode_call(self):
        self.assertRaises(TypeError, pinktrace.socket.decode_call)
        self.assertRaises(TypeError, pinktrace.socket.decode_call, 'pink')
        self.assertRaises(ValueError, pinktrace.socket.decode_call, 0, 13)

    def test_03_decode_fd(self):
        self.assertRaises(TypeError, pinktrace.socket.decode_fd)
        self.assertRaises(IndexError, pinktrace.socket.decode_fd, 0, pinktrace.syscall.MAX_INDEX)
        self.assertRaises(ValueError, pinktrace.socket.decode_fd, 0, 1, 13)

    def test_04_decode_address(self):
        self.assertRaises(TypeError, pinktrace.socket.decode_address)
        self.assertRaises(TypeError, pinktrace.socket.decode_address, 0)
        self.assertRaises(IndexError, pinktrace.socket.decode_address, 0, pinktrace.syscall.MAX_INDEX)
        self.assertRaises(ValueError, pinktrace.socket.decode_address, 0, 1, 13)

## FIXME: The test cases below must be skipped if system call names weren't generated.
TEST_UNIX_SOCKET = './TEST_UNIX_SOCKET'
class TestSocket_02(unittest.TestCase):

    def test_01_decode_call(self):
        pid = pinktrace.fork.fork()
        if pid == 0: # child
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            os._exit(0)
        else: # parent
            # Loop until we get to the socket() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            while ev != pinktrace.event.EVENT_EXIT_GENUINE:
                pinktrace.trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = pinktrace.event.decide(status)
                if ev == pinktrace.event.EVENT_SYSCALL:
                    scno = pinktrace.syscall.get_no(pid)
                    name = pinktrace.syscall.name(scno)
                    if name == 'socketcall':
                        # Decode the call to its subcall
                        subno = pinktrace.socket.decode_call(pid)
                        subname = pinktrace.socket.name(subname)
                        self.assertEqual(subname, 'socket')
                        break
                    elif name == 'socket':
                        break

            try: pinktrace.trace.kill(pid)
            except OSError: pass

    def test_02_decode_fd(self):
        pid = pinktrace.fork.fork()
        if pid == 0: # child
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            os._exit(0)
        else: # parent
            # Loop until we get to the socket() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            while ev != pinktrace.event.EVENT_EXIT_GENUINE:
                pinktrace.trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = pinktrace.event.decide(status)
                if ev == pinktrace.event.EVENT_SYSCALL:
                    scno = pinktrace.syscall.get_no(pid)
                    name = pinktrace.syscall.name(scno)
                    if name == 'socketcall' or name == 'socket':
                        fd = pinktrace.socket.decode_fd(pid)
                        self.assert_(fd > 0)

            try: pinktrace.trace.kill(pid)
            except OSError: pass

    def test_03_decode_address_unix(self):
        pid = pinktrace.fork.fork()
        if pid == 0: # child
            s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            s.connect(TEST_UNIX_SOCKET)
        else: # parent
            # Loop until we get to the connect() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            addr = None
            while ev != pinktrace.event.EVENT_EXIT_GENUINE:
                pinktrace.trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = pinktrace.event.decide(status)
                if ev == pinktrace.event.EVENT_SYSCALL:
                    scno = pinktrace.syscall.get_no(pid)
                    name = pinktrace.syscall.name(scno)
                    if name == 'socketcall':
                        subno = pintrace.socket.decode_call(pid)
                        subname = pinktrace.socket.name(subno)
                        if subname == 'connect':
                            addr = pinktrace.socket.decode_address(pid, 1)
                            break
                    elif name == 'connect':
                        addr = pinktrace.socket.decode_address(pid, 1)
                        break

            self.assert_(isinstance(addr, pinktrace.socket.Address), "%r" % addr)
            self.assertEqual(addr.family, socket.AF_UNIX)
            self.assertEqual(addr.path, TEST_UNIX_SOCKET)
            self.assertEqual(addr.ip, None)

            try: pinktrace.trace.kill(pid)
            except OSError: pass

    def test_04_decode_address_inet(self):
        pid = pinktrace.fork.fork()
        if pid == 0: # child
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.connect(("127.0.0.1", 12345))
        else: # parent
            # Loop until we get to the connect() system call as there's no
            # guarantee that other system calls won't be called beforehand.
            ev = -1
            addr = None
            while ev != pinktrace.event.EVENT_EXIT_GENUINE:
                pinktrace.trace.syscall(pid)
                pid, status = os.waitpid(pid, 0)

                ev = pinktrace.event.decide(status)
                if ev == pinktrace.event.EVENT_SYSCALL:
                    scno = pinktrace.syscall.get_no(pid)
                    name = pinktrace.syscall.name(scno)
                    if name == 'socketcall':
                        subno = pintrace.socket.decode_call(pid)
                        subname = pinktrace.socket.name(subno)
                        if subname == 'connect':
                            addr = pinktrace.socket.decode_address(pid, 1)
                            break
                    elif name == 'connect':
                        addr = pinktrace.socket.decode_address(pid, 1)
                        break

            self.assert_(isinstance(addr, pinktrace.socket.Address), "%r" % addr)
            self.assertEqual(addr.family, socket.AF_INET)
            self.assertEqual(addr.ip, "127.0.0.1")
            self.assertEqual(addr.path, None)

            try: pinktrace.trace.kill(pid)
            except OSError: pass

if __name__ == '__main__':
    unittest.main()
