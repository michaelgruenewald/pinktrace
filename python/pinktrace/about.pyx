# vim: set sw=4 ts=4 sts=4 et :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

# Process this file with Cython.

cdef extern from "pinktrace/pink.h":
    cdef enum:
        PINKTRACE_VERSION
        PINKTRACE_VERSION_MAJOR
        PINKTRACE_VERSION_MINOR
        PINKTRACE_VERSION_MICRO
    char *PINKTRACE_VERSION_SUFFIX
    char *PINKTRACE_GIT_HEAD
