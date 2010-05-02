# vim: set sw=4 ts=4 sts=4 et :
# Copyright 2010 Ali Polatel <alip@exherbo.org>
# Distributed under the terms of the GNU Lesser General Public License v2.1

# Process this file with Cython.

cdef extern from "errno.h":
    int errno

cdef extern from "stdbool.h":
    int true

cdef extern from "string.h":
    char *strerror(int errnum)

cdef extern from "pinktrace/pink.h":
    cdef enum:
        PINK_MAX_INDEX

    char *PINKTRACE_ARCHITECTURE

    char *pink_name_syscall(long scno, int bit)

    int pink_util_get_syscall(int pid, long *res)
    int pink_util_set_syscall(int pid, long scno)
    int pink_util_get_return(int pid, long *res)
    int pink_util_set_return(int pid, long ret)
    int pink_util_get_arg(int pid, int bit, unsigned ind, long *res)

import pinktrace.bitness

MAX_INDEX = PINK_MAX_INDEX
__ARCHITECTURE = PINKTRACE_ARCHITECTURE

def name(scno, bitness=pinktrace.bitness.DEFAULT_BITNESS):
    """
    Return the name of the given system call.

    Note: This call depends on the generated system call names.
    You can check whether they are generated with:

        if pinktrace.syscall.name(0) is None:
            # Names weren't generated
        else:
            # Names were generated

    @param scno: System call number
    @param bitness: The bitness of the traced child
    @return: The name of the system call or None.
    """

    cdef char *scname

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

    scname = pink_name_syscall(scno, bitness)
    if scname is NULL:
        return None
    return scname

def get_no(pid):
    """
    Returns the last system call number called by the traced child.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the traced child
    @rtype: long
    @return: The number of the system call
    """

    cdef long scno

    if pink_util_get_syscall(pid, &scno) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

    return scno

def set_no(pid, scno):
    """
    Sets the number of the last system call called by the traced child.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the traced child
    @param scno: The number of the system call
    """

    if pink_util_set_syscall(pid, scno) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def get_ret(pid):
    """
    Returns the return value of the last system call called by the traced child.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the traced child
    @rtype: long
    @return: The return value
    """

    cdef long ret

    if pink_util_get_return(pid, &ret) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

    return ret

def set_ret(pid, ret):
    """
    Sets the return value of the last system call called by the traced child.

    Note: This function raises OSError if the call fails.

    @param pid: Process ID of the traced child
    @param ret: The return value
    """

    if pink_util_set_return(pid, ret) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

def get_arg(pid, index, bitness=pinktrace.bitness.DEFAULT_BITNESS):
    """
    Returns the system call argument at the given index.

    Note: This function raises OSError if the call fails.

    Note: This function raises IndexError if the index is not smaller than
    pinktrace.syscall.MAX_INDEX

    Note: This function raises pinktrace.bitness.BitnessError if the given
    bitness is either unsupported or undefined.

    @param pid: Process ID of the traced child
    @param index: The index of the argument
    @param bitness: The bitness of the traced child
    """
    cdef long arg

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

    if index >= MAX_INDEX:
        raise IndexError("Unsupported index: %d", index)

    if pink_util_get_arg(pid, bitness, index, &arg) != true:
        raise OSError("[Errno %d] %s" % (errno, strerror(errno)))

    return arg
