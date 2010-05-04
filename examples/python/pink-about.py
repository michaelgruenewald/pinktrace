#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et :

"""
Simple example showing how to use PinkTrace version constants
"""

from __future__ import print_function

import pinktrace

print("Built using PinkTrace %d.%d.%d%s" % (pinktrace.VERSION_MAJOR,
    pinktrace.VERSION_MINOR, pinktrace.VERSION_MICRO,
    pinktrace.VERSION_SUFFIX), end="")

if pinktrace.GIT_HEAD:
    print(" git-%s" % pinktrace.GIT_HEAD)
else:
    print()
