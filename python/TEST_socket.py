#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et :

import os, signal, socket, sys, unittest

sys.path.insert(0, '.')
import pinktrace
import pinktrace.event
import pinktrace.socket
import pinktrace.syscall
import pinktrace.trace

UNAME = os.uname()

class TestSocket_01_Invalid(unittest.TestCase):

    def test_01_name(self):
        if UNAME[0] != 'Linux': return

        self.assertRaises(TypeError, pinktrace.socket.name)

    def test_02_decode_call(self):
        if UNAME[0] != 'Linux': return

        self.assertRaises(TypeError, pinktrace.socket.decode_call)
        self.assertRaises(TypeError, pinktrace.socket.decode_call, 'pink')
        self.assertRaises(ValueError, pinktrace.socket.decode_call, 0, 13)

    def test_03_decode_fd(self):
        if UNAME[0] != 'Linux': return

        self.assertRaises(TypeError, pinktrace.socket.decode_fd)
        self.assertRaises(IndexError, pinktrace.socket.decode_fd, 0, pinktrace.syscall.MAX_INDEX)
        self.assertRaises(ValueError, pinktrace.socket.decode_fd, 0, 1, 13)

    def test_04_decode_address(self):
        self.assertRaises(TypeError, pinktrace.socket.decode_address)
        self.assertRaises(TypeError, pinktrace.socket.decode_address, 0)
        self.assertRaises(IndexError, pinktrace.socket.decode_address, 0, pinktrace.syscall.MAX_INDEX)
        self.assertRaises(ValueError, pinktrace.socket.decode_address, 0, 1, 13)

if __name__ == '__main__':
    unittest.main()
