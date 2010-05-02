#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set sw=4 ts=4 sts=4 et tw=80 :

import sys, unittest

sys.path.insert(0, '.')
from pinktrace import trace

class TestTraceInvalid(unittest.TestCase):

    #def testTraceMe(self):
    #    trace.me()
    #    self.assertRaises(SystemError, trace.me)

    def testTraceCont(self):
        self.assertRaises(TypeError, trace.cont)
        self.assertRaises(TypeError, trace.cont, 'pink')
        self.assertRaises(TypeError, trace.cont, 0, 'pink')
        self.assertRaises(SystemError, trace.cont, 0)

    def testTraceKill(self):
        self.assertRaises(TypeError, trace.kill)
        self.assertRaises(TypeError, trace.kill, 'pink')
        self.assertRaises(SystemError, trace.kill, 0)

    def testTraceSingleStep(self):
        self.assertRaises(TypeError, trace.singlestep)
        self.assertRaises(TypeError, trace.singlestep, 'pink')
        self.assertRaises(TypeError, trace.singlestep, 0, 'pink')
        self.assertRaises(SystemError, trace.singlestep, 0)

    def testTraceSyscall(self):
        self.assertRaises(TypeError, trace.syscall)
        self.assertRaises(TypeError, trace.syscall, 'pink')
        self.assertRaises(TypeError, trace.syscall, 0, 'pink')
        self.assertRaises(SystemError, trace.syscall, 0)

    def testTraceGetEventMsg(self):
        self.assertRaises(TypeError, trace.geteventmsg)
        self.assertRaises(TypeError, trace.geteventmsg, 'pink')
        self.assertRaises(SystemError, trace.geteventmsg, 0)

    def testTraceSetup(self):
        self.assertRaises(TypeError, trace.setup)
        self.assertRaises(TypeError, trace.setup, 'pink')
        self.assertRaises(TypeError, trace.setup, 0, 'pink')
        self.assertRaises(SystemError, trace.setup, 0)

    def testTraceAttach(self):
        self.assertRaises(TypeError, trace.attach)
        self.assertRaises(TypeError, trace.attach, 'pink')
        self.assertRaises(TypeError, trace.attach, 0, 'pink')
        self.assertRaises(SystemError, trace.attach, 0)

    def testTraceDetach(self):
        self.assertRaises(TypeError, trace.detach)
        self.assertRaises(TypeError, trace.detach, 'pink')
        self.assertRaises(TypeError, trace.detach, 0, 'pink')
        self.assertRaises(SystemError, trace.detach, 0)

if __name__ == '__main__':
    unittest.main()
