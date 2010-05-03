#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et tw=80 :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

"""\
pinktrace Python bindings
"""

__author__ = "Ali Polatel <alip@exherbo.org>"
__license__ = "LGPL-2.1"

__all__ = ('about', 'bitness', 'event', 'fork', 'socket', 'string', 'syscall', 'trace')

import pinktrace.about
from pinktrace.about import *

import pinktrace.bitness
import pinktrace.event
import pinktrace.fork
import pinktrace.socket
import pinktrace.string
import pinktrace.syscall
import pinktrace.trace
