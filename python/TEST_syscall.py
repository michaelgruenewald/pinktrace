#!/usr/bin/env python
# coding: utf-8

import sys, unittest

sys.path.insert(0, '.')
from pinktrace import syscall

class TestSyscall_01_Invalid(unittest.TestCase):

    def test_01_name(self):
        self.assertRaises(TypeError, syscall.name)
        self.assertRaises(ValueError, syscall.name, 0, 13)

    def test_02_get_no(self):
        self.assertRaises(TypeError, syscall.get_no)
        self.assertRaises(OSError, syscall.get_no, 0)

    def test_03_set_no(self):
        self.assertRaises(TypeError, syscall.set_no)
        self.assertRaises(TypeError, syscall.set_no, 0)
        self.assertRaises(OSError, syscall.set_no, 0, 1)

    def test_04_get_ret(self):
        self.assertRaises(TypeError, syscall.get_ret)
        self.assertRaises(OSError, syscall.get_ret, 0)

    def test_05_set_ret(self):
        self.assertRaises(TypeError, syscall.set_ret)
        self.assertRaises(TypeError, syscall.set_ret, 0)
        self.assertRaises(OSError, syscall.set_ret, 0, 1)

    def test_06_get_arg(self):
        self.assertRaises(TypeError, syscall.get_arg)
        self.assertRaises(TypeError, syscall.get_arg, 0)
        self.assertRaises(IndexError, syscall.get_arg, 0, syscall.MAX_INDEX)
        self.assertRaises(ValueError, syscall.get_arg, 0, 1, 13)

if __name__ == '__main__':
    unittest.main()
