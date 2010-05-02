#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: set sw=4 ts=4 sts=4 et tw=80 :

import os, sys, unittest

sys.path.insert(0, '.')
from pinktrace import bitness, fork, trace

class TestBitness_01_Invalid(unittest.TestCase):
    def test_01_get(self):
        self.assertRaises(TypeError, bitness.get)
        self.assertRaises(TypeError, bitness.get, 'pink')

class TestBitness_02(unittest.TestCase):
    def test_02_get(self):
        pid = fork.fork()
        if (pid == 0): # child
            os._exit(13)
        else: # parent
            bit = bitness.get(pid)
            self.assertEqual(bit, bitness.DEFAULT_BITNESS)

            try: trace.kill(pid)
            except OSError: pass

if __name__ == '__main__':
    unittest.main()
