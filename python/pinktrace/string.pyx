# vim: set sw=4 ts=4 sts=4 et :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

# Process this file with Cython.

cdef extern from "errno.h":
    int errno

cdef extern from 'stdbool.h':
    int true

cdef extern from "string.h":
    char *strerror(int errnum)

cdef extern from "stdlib.h":
    void *free(void *ptr)
    # We declare malloc() return as char* here otherwise Cython barfs.
    char *malloc(size_t size)

cdef extern from "Python.h":
    object PyString_FromString(char *string)
    object PyString_FromStringAndSize(char *string, Py_ssize_t length)
    Py_ssize_t PyString_Size(object string)

cdef extern from "pinktrace/pink.h":
    cdef enum:
        PINK_MAX_INDEX

    char *PINKTRACE_ARCHITECTURE

    int pink_decode_string(int pid, int bitness, unsigned ind, char *dest, size_t length)
    char *pink_decode_string_persistent(int pid, int bitness, unsigned ind)

    int pink_encode_simple(int pid, int bitness, unsigned ind, char *src, size_t length)
    int pink_encode_simple_safe(int pid, int bitness, unsigned ind, char *src, size_t length)

__MAX_INDEX = PINK_MAX_INDEX
__ARCHITECTURE = PINKTRACE_ARCHITECTURE

import pinktrace.bitness

def decode(pid, index, maxlen=-1, bitness=pinktrace.bitness.DEFAULT_BITNESS):
    """
    This function decodes the string at the argument of the given index.

    Note: This function raises OSError if the call fails.

    Note: This function raises IndexError if the index is not smaller than
    pinktrace.syscall.MAX_INDEX

    Note: This function raises pinktrace.bitness.BitnessError if the given
    bitness is either unsupported or undefined.

    @param pid: Process ID of the traced child
    @param index: The index of the argument
    @param maxlen: Max length of the string (if smaller than zero (default),
    pinktrace tries to determine the length itself)
    @param bitness: The bitness of the traced child
    @rtype: str
    @return: The decoded string
    """

    cdef char *string
    cdef object obj

    # Ugly hack, we don't want to edit the resulting C file (ugh!) or use
    # UNAME_MACHINE, because the idea is to distribute the generated C file.
    if __ARCHITECTURE == "x86_64":
        if bitness != pinktrace.bitness.BITNESS_32 and bitness != pinktrace.bitness.BITNESS_64:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)
    elif __ARCHITECTURE in ("i386", "powerpc"):
        if bitness != pinktrace.bitness.BITNESS_32:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)
    elif __ARCHITECTURE in ("ia64", "powerpc64"):
        if bitness != pinktrace.bitness.BITNESS_64:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)

    if index >= __MAX_INDEX:
        raise IndexError("Unsupported index: %d", index)

    if maxlen < 0:
        string = pink_decode_string_persistent(pid, bitness, index)
        if string == NULL:
            raise OSError("[Errno %d] %s" % (errno, strerror(errno)))
        obj = PyString_FromString(string)
        free(string)
        return obj

    string = malloc(sizeof(char) * maxlen)
    if string == NULL:
        raise MemoryError("[Errno %d] %s" % (errno, strerror(errno)))

    if pink_decode_string(pid, bitness, index, string, maxlen) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))
    obj = PyString_FromStringAndSize(string, maxlen)
    free(string)
    return obj

def encode(pid, index, string, bitness=pinktrace.bitness.DEFAULT_BITNESS):
    """
    Encode a string into the argument of the given index safely.

    Note: This function raises OSError if the call fails.

    Note: This function raises IndexError if the index is not smaller than
    pinktrace.syscall.MAX_INDEX

    Note: This function raises pinktrace.bitness.BitnessError if the given
    bitness is either unsupported or undefined.

    @param pid: Process ID of the traced child
    @param index: The index of the argument
    @param string: The string to be encoded
    @param bitness: The bitness of the traced child
    """

    cdef Py_ssize_t length

    # Ugly hack, we don't want to edit the resulting C file (ugh!) or use
    # UNAME_MACHINE, because the idea is to distribute the generated C file.
    if __ARCHITECTURE == "x86_64":
        if bitness != pinktrace.bitness.BITNESS_32 and bitness != pinktrace.bitness.BITNESS_64:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)
    elif __ARCHITECTURE in ("i386", "powerpc"):
        if bitness != pinktrace.bitness.BITNESS_32:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)
    elif __ARCHITECTURE in ("ia64", "powerpc64"):
        if bitness != pinktrace.bitness.BITNESS_64:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)

    if index >= __MAX_INDEX:
        raise IndexError("Unsupported index: %d", index)

    length = PyString_Size(string)

    if pink_encode_simple_safe(pid, bitness, index, string, length) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def encode_unsafe(pid, index, string, bitness=pinktrace.bitness.DEFAULT_BITNESS):
    """
    Encode a string into the argument of the given index without additional
    checking for writable areas.

    Note: This function raises OSError if the call fails.

    Note: This function raises IndexError if the index is not smaller than
    pinktrace.syscall.MAX_INDEX

    Note: This function raises pinktrace.bitness.BitnessError if the given
    bitness is either unsupported or undefined.

    @param pid: Process ID of the traced child
    @param index: The index of the argument
    @param string: The string to be encoded
    @param bitness: The bitness of the traced child
    """

    cdef Py_ssize_t length

    # Ugly hack, we don't want to edit the resulting C file (ugh!) or use
    # UNAME_MACHINE, because the idea is to distribute the generated C file.
    if __ARCHITECTURE == "x86_64":
        if bitness != pinktrace.bitness.BITNESS_32 and bitness != pinktrace.bitness.BITNESS_64:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)
    elif __ARCHITECTURE in ("i386", "powerpc"):
        if bitness != pinktrace.bitness.BITNESS_32:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)
    elif __ARCHITECTURE in ("ia64", "powerpc64"):
        if bitness != pinktrace.bitness.BITNESS_64:
            raise pinktrace.bitness.BitnessError("Unsupported bitness: %d" % bitness)

    if index >= __MAX_INDEX:
        raise IndexError("Unsupported index: %d", index)

    length = PyString_Size(string)

    if pink_encode_simple(pid, bitness, index, string, length) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))
