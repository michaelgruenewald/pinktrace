#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et :

import os, signal, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, trace

class TestBitness_01_Invalid(unittest.TestCase):
    def test_01_get(self):
        self.assertRaises(TypeError, bitness.get)
        self.assertRaises(TypeError, bitness.get, 'pink')

class TestBitness_02(unittest.TestCase):
    def test_02_get(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)
            os._exit(13)
        else: # parent
            os.waitpid(pid, 0)

            bit = bitness.get(pid)
            self.assertEqual(bit, bitness.DEFAULT)

            try: trace.kill(pid)
            except OSError: pass

if __name__ == '__main__':
    unittest.main()
