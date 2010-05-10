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

#ifndef PINKTRACE_GUARD_PROC_DECODE_H
#define PINKTRACE_GUARD_PROC_DECODE_H 1

#if defined(PINKTRACE_FREEBSD) || defined(DOXYGEN)

#include <stdbool.h>
#include <sys/types.h>

#include <pinktrace/bitness.h>

/**
 * \file
 * Pink's proc system call decoders
 *
 * Availability: FreeBSD
 **/

/**
 * Get the data in argument arg and place it in dest.
 *
 * \param fd Main /proc file descriptor (returned by pink_proc_open())
 * \param rfd /proc file descriptor (returned by pink_proc_util_open())
 * \param bitness Bitness of the child
 * \param ind The index of the argument (0-5)
 * \param dest Pointer to store the data
 * \param len Length of the data
 **/
bool
pink_proc_decode_simple(int fd, int rfd, pink_bitness_t bitness, unsigned ind, void *dest, size_t len);

/**
 * Get the string argument and place it in dest.
 *
 * \note On FreeBSD this function is equivalent to pink_proc_decode_simple().
 *
 * \param fd Main /proc file descriptor (returned by pink_proc_open())
 * \param rfd /proc file descriptor (returned by pink_proc_util_open())
 * \param bitness Bitness of the child
 * \param ind The index of the argument (0-5, see #PINK_MAX_INDEX)
 * \param dest Pointer to store the string
 * \param len Length of the string
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_decode_string(int fd, int rfd, pink_bitness_t bitness, unsigned ind, char *dest, size_t len);

/**
 * Like pink_proc_decode_string() but allocates the string itself.
 *
 * \return The path on success, NULL on failure and sets errno
 * accordingly.
 **/
char *
pink_proc_decode_string_persistent(int fd, int rfd, pink_bitness_t bitness, unsigned ind);

#endif /* defined(PINKTRACE_FREEBSD)... */
#endif /* !PINKTRACE_GUARD_PROC_DECODE_H */
