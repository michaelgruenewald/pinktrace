/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 *
 * This file is part of Pink's Tracing Library. pinktrace is free software; you
 * can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License version 2.1, as published by the Free Software
 * Foundation.
 *
 * pinktrace is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef PINKTRACE_GUARD_PROC_UTIL_H
#define PINKTRACE_GUARD_PROC_UTIL_H 1

#if defined(PINKTRACE_FREEBSD) || defined(DOXYGEN)

#include <stdbool.h>

#include <sys/types.h>
#include <machine/reg.h>
#include <sys/pioctl.h>

/**
 * \file Pink's utilities for /proc & ioctl based tracing
 *
 * Availability: FreeBSD
 **/

/**
 * Open the file descriptor required to read reagisters.
 *
 * \param pid Process ID of the child, if this argument is smaller than zero,
 * /proc/curproc/regs is opened instead of /proc/$pid/regs.
 * \param fd Address to save the /proc file descriptor.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_open(pid_t pid, int *rfd);

/**
 * Copy the child's general purpose registers to the given location.
 *
 * \param rfd /proc file descriptor
 * \param regs Address to store the structure of registers.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_get_regs(int rfd, struct reg *regs);

/**
 * Set the child's general purpose registers.
 *
 * \param rfd /proc file descriptor
 * \param regs Same as pink_proc_util_get_regs()
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_set_regs(int rfd, const struct reg *regs);

/**
 * Return the bitness of the given process ID.
 *
 * \param pid Process ID of the traced child.
 *
 * \return One of PINK_BITNESS_* constants.
 **/
pink_bitness_t
pink_proc_util_get_bitness(pid_t pid);

/**
 * Gets the last system call called by child with the given process ID.
 *
 * \param fd Main /proc file descriptor (returned by pink_proc_open())
 * \param rfd /proc file descriptor (returned by pink_proc_util_open())
 * \param bitness Bitness of the child
 * \param res Address to store the result.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_get_syscall(int fd, int rfd, pink_bitness_t bitness, long *res);

/**
 * Sets the system call to the given value.
 *
 * \param fd Main /proc file descriptor (returned by pink_proc_open())
 * \param rfd /proc file descriptor (returned by pink_proc_util_open())
 * \param bitness Bitness of the child
 * \param scno System call to set.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_set_syscall(int fd, int rfd, pink_bitness_t bitness, long scno);

/**
 * Gets the return value of the last system call called by child with the given
 * process ID.
 *
 * \param rfd /proc file descriptor (returned by pink_proc_util_open())
 * \param res Pointer to store the result.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_get_return(int rfd, long *res);

/**
 * Sets the return value of the last system call called by child with the given
 * process ID.
 *
 * \param rfd /proc file descriptor (returned by pink_proc_util_open())
 * \param ret Return value to set.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_set_return(int rfd, long ret);

/**
 * Get the given argument and place it in res.
 *
 * \param fd Main /proc file descriptor (returned by pink_proc_open())
 * \param rfd /proc file descriptor (returned by pink_proc_util_open())
 * \param bitness Bitness of the child
 * \param ind The index of the argument (0-5, see #pink_proc_MAX_INDEX)
 * \param res Pointer to store the argument
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_util_get_arg(int fd, int rfd, pink_bitness_t bitness, unsigned ind, unsigned long *res);

#endif /* defined(FREEBSD)... */
#endif /* !PINKTRACE_GUARD_PROC_UTIL_H */
