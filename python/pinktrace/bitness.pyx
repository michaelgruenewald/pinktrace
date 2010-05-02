# vim: set sw=4 ts=4 sts=4 et :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

# Process this file with Cython.

cdef extern from "errno.h":
    int errno

cdef extern from "string.h":
    char *strerror(int errnum)

cdef extern from "pinktrace/pink.h":
    cdef enum:
        PINK_BITNESS_32
        PINK_BITNESS_64
        PINK_BITNESS_UNKNOWN

    cdef enum:
        PINKTRACE_DEFAULT_BITNESS
        PINKTRACE_SUPPORTED_BITNESS

    int pink_bitness_get(int pid)

BITNESS_32 = PINK_BITNESS_32
BITNESS_64 = PINK_BITNESS_64
DEFAULT_BITNESS = PINKTRACE_DEFAULT_BITNESS
SUPPORTED_BITNESS = PINKTRACE_SUPPORTED_BITNESS

class BitnessError(RuntimeError):
    """Raised when the bitness argument of a function is unknown."""
    pass

def get(pid):
    """
    Returns the bitness of the given process ID.

    Note: This function raises OSError if the bitness of the process can't be
    determined.

    @param pid: Process ID of the process whose bitness is to be returned.
    @rtype: int
    @return: One of pinktarace.bitness.BITNESS_* constants
    """

    cdef int bit

    bit = pink_bitness_get(pid)
    if bit == PINK_BITNESS_UNKNOWN:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

    return bit
