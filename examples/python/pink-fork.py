#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et :

"""\
An example demonstrating the tracing fork
"""

from __future__ import print_function

import pinktrace.fork
import pinktrace.trace

pid = pinktrace.fork.fork(pinktrace.trace.OPTION_ALL)
if not pid: # child
    print("hello world")
else: # parent
    # At this point the child has been stopped for tracing and stopped itself
    # using SIGSTOP. We don't do anything interesting for this example.

    # Kill the child, the print function will never be called.
    pinktrace.trace.kill(pid)
