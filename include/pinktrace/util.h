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

#ifndef PINKTRACE_GUARD_UTIL_H
#define PINKTRACE_GUARD_UTIL_H 1

/**
 * \file
 * Pink's trace utility functions
 **/

#include <stdbool.h>
#include <sys/types.h>

/**
 * Reads a word at the given offset in the child's USER area,
 * and places it in res.
 *
 * \note Mostly for internal use, use higher level functions where possible.
 *
 * \param pid Process ID of the child whose USER area is to be read.
 * \param off Offset
 * \param res Result
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_peek(pid_t pid, long off, long *res);

/**
 * Copies the given word data to the given offset in the child's USER area.
 *
 * \note Mostly for internal use, use higher level functions where possible.
 *
 * \param pid Process ID of the child.
 * \param off Offset
 * \param val Word
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_poke(pid_t pid, long off, long val);

/**
 * Move len bytes of data of process pid, at address addr, to our address space
 * dest.
 *
 * \note Mostly for internal use, use higher level functions where possible.
 *
 * \param pid Process ID of the child.
 * \param addr Address where the data is to be moved from.
 * \param dest Pointer to store the data.
 * \param len Number of bytes of data to move.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_moven(pid_t pid, long addr, char *dest, size_t len);

/**
 * Convenience macro to read an object
 *
 * \note Mostly for internal use, use higher level functions where possible.
 *
 * \see pink_util_moven
 **/
#define pink_util_move(pid, addr, objp) \
	pink_util_moven((pid), (addr), (char *)(objp), sizeof *(objp))

/**
 * Like pink_util_moven() but make the additional effort of looking for a
 * terminating zero-byte.
 *
 * \note Mostly for internal use, use higher level functions where possible.
 **/
bool
pink_util_movestr(pid_t pid, long addr, char *dest, size_t len);

/**
 * Like pink_util_movestr() but allocates the string itself.
 *
 * \note Mostly for internal use, use higher level functions where possible.
 *
 * \return The string on success and NULL on failure and sets errno
 * accordingly.
 **/
char *
pink_util_movestr_persistent(pid_t pid, long addr);

/**
 * Copy len bytes of data to process pid, at address addr, from our address space
 * src.
 *
 * \note Mostly for internal use, use higher level functions where possible.
 *
 * \param pid Process ID of the child being traced
 * \param addr Address where the data is to be copied to
 * \param src Pointer to the data to be moved
 * \param len Length of data
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_putn(pid_t pid, long addr, const char *src, size_t len);

/**
 * Convenience macro to write an object
 *
 * \note Mostly for internal use, use higher level functions where possible.
 *
 * \see pink_util_putn
 **/
#define pink_util_put(pid, addr, objp) \
	pink_util_putn((pid), (addr), (const char *)(objp), sizeof *(objp))

/**
 * Gets the last system call called by child with the given process ID.
 *
 * \param pid Process ID of the child whose system call is to be returned.
 * \param res Pointer to store the result.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_get_syscall(pid_t pid, long *res);

/**
 * Sets the system call to the given value.
 *
 * \param pid Process ID of the child whose system call is to be set.
 * \param scno System call to set.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_set_syscall(pid_t pid, long scno);

/**
 * Gets the return value of the last system call called by child with the given
 * process ID.
 *
 * \param pid Process ID of the child whose system call return value is to be
 * returned.
 * \param res Pointer to store the result.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_get_return(pid_t pid, long *res);

/**
 * Sets the return value of the last system call called by child with the given
 * process ID.
 *
 * \param pid Process ID of the child whose system call return value is to be
 * modified.
 * \param ret Return value to set.
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_set_return(pid_t pid, long ret);

/**
 * Get the given argument and place it in res.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param arg The number of the argument (0-5)
 * \param res Pointer to store the argument
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_util_get_arg(pid_t pid, pink_bitness_t bitness, int arg, long *res);

#endif /* !PINKTRACE_GUARD_UTIL_H */
