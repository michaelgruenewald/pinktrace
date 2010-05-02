#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set sw=4 ts=4 sts=4 et tw=80 :

import sys, unittest

sys.path.insert(0, '.')
from pinktrace import trace

class TestTrace_01_Invalid(unittest.TestCase):

    #def testTraceMe(self):
    #    trace.me()
    #    self.assertRaises(OSError, trace.me)

    def test_01_cont(self):
        self.assertRaises(TypeError, trace.cont)
        self.assertRaises(TypeError, trace.cont, 'pink')
        self.assertRaises(TypeError, trace.cont, 0, 'pink')
        self.assertRaises(OSError, trace.cont, 0)

    def test_02_kill(self):
        self.assertRaises(TypeError, trace.kill)
        self.assertRaises(TypeError, trace.kill, 'pink')
        self.assertRaises(OSError, trace.kill, 0)

    def test_03_singlestep(self):
        self.assertRaises(TypeError, trace.singlestep)
        self.assertRaises(TypeError, trace.singlestep, 'pink')
        self.assertRaises(TypeError, trace.singlestep, 0, 'pink')
        self.assertRaises(OSError, trace.singlestep, 0)

    def test_04_syscall(self):
        self.assertRaises(TypeError, trace.syscall)
        self.assertRaises(TypeError, trace.syscall, 'pink')
        self.assertRaises(TypeError, trace.syscall, 0, 'pink')
        self.assertRaises(OSError, trace.syscall, 0)

    def test_05_geteventmsg(self):
        self.assertRaises(TypeError, trace.geteventmsg)
        self.assertRaises(TypeError, trace.geteventmsg, 'pink')
        self.assertRaises(OSError, trace.geteventmsg, 0)

    def test_06_setup(self):
        self.assertRaises(TypeError, trace.setup)
        self.assertRaises(TypeError, trace.setup, 'pink')
        self.assertRaises(TypeError, trace.setup, 0, 'pink')
        self.assertRaises(OSError, trace.setup, 0)

    def test_07_attach(self):
        self.assertRaises(TypeError, trace.attach)
        self.assertRaises(TypeError, trace.attach, 'pink')
        self.assertRaises(TypeError, trace.attach, 0, 'pink')
        self.assertRaises(OSError, trace.attach, 0)

    def test_08_detach(self):
        self.assertRaises(TypeError, trace.detach)
        self.assertRaises(TypeError, trace.detach, 'pink')
        self.assertRaises(TypeError, trace.detach, 0, 'pink')
        self.assertRaises(OSError, trace.detach, 0)

if __name__ == '__main__':
    unittest.main()
