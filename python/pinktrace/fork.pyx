# vim: set sw=4 ts=4 sts=4 et :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

# Process this file with Cython.

cdef extern from "errno.h":
    int errno

cdef extern from "string.h":
    char *strerror(int errnum)

cdef extern from "pinktrace/pink.h":
    char *pink_error_tostring(int error)
    int pink_fork(int options, int *error_r)

import pinktrace.trace

def fork(options=pinktrace.trace.OPTION_SYSGOOD):
    """
    fork(2) wrapper that sets up the child for tracing.

    Note: This function raises OSError if an error occurs.

    @param options: Bitwise OR'ed pinktrace.trace.OPTION_* flags
    @rtype: int
    @return: Return 0 in the child and the child's process ID in the parent.
    """

    cdef int error, pid

    pid = pink_fork(options, &error)
    if pid < 0:
        raise OSError("[Errno %d] %s (%s)" % (errno, pink_error_tostring(error), strerror(errno)))
    return pid
