#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set sw=4 ts=4 sts=4 et tw=80 :

import sys, unittest

sys.path.insert(0, '.libs')
import pinktrace

class TestTraceInvalid(unittest.TestCase):

    #def testTraceMe(self):
    #    pinktrace.trace_me()
    #    self.assertRaises(SystemError, pinktrace.trace_me)

    def testTraceCont(self):
        self.assertRaises(TypeError, pinktrace.trace_cont)
        self.assertRaises(TypeError, pinktrace.trace_cont, 'pink')
        self.assertRaises(TypeError, pinktrace.trace_cont, 0, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_cont, 0)

    def testTraceKill(self):
        self.assertRaises(TypeError, pinktrace.trace_kill)
        self.assertRaises(TypeError, pinktrace.trace_kill, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_kill, 0)

    def testTraceSingleStep(self):
        self.assertRaises(TypeError, pinktrace.trace_singlestep)
        self.assertRaises(TypeError, pinktrace.trace_singlestep, 'pink')
        self.assertRaises(TypeError, pinktrace.trace_singlestep, 0, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_singlestep, 0)

    def testTraceSyscall(self):
        self.assertRaises(TypeError, pinktrace.trace_syscall)
        self.assertRaises(TypeError, pinktrace.trace_syscall, 'pink')
        self.assertRaises(TypeError, pinktrace.trace_syscall, 0, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_syscall, 0)

    def testTraceGetEventMsg(self):
        self.assertRaises(TypeError, pinktrace.trace_geteventmsg)
        self.assertRaises(TypeError, pinktrace.trace_geteventmsg, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_geteventmsg, 0)

    def testTraceSetup(self):
        self.assertRaises(TypeError, pinktrace.trace_setup)
        self.assertRaises(TypeError, pinktrace.trace_setup, 'pink')
        self.assertRaises(TypeError, pinktrace.trace_setup, 0, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_setup, 0)

    def testTraceAttach(self):
        self.assertRaises(TypeError, pinktrace.trace_attach)
        self.assertRaises(TypeError, pinktrace.trace_attach, 'pink')
        self.assertRaises(TypeError, pinktrace.trace_attach, 0, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_attach, 0)

    def testTraceDetach(self):
        self.assertRaises(TypeError, pinktrace.trace_detach)
        self.assertRaises(TypeError, pinktrace.trace_detach, 'pink')
        self.assertRaises(TypeError, pinktrace.trace_detach, 0, 'pink')
        self.assertRaises(SystemError, pinktrace.trace_detach, 0)

if __name__ == '__main__':
    unittest.main()
