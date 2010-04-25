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

#ifndef PINKTRACE_GUARD_DECODE_H
#define PINKTRACE_GUARD_DECODE_H 1

/**
 * \file
 * Pink's system call decoders
 **/

#include <stdbool.h>
#include <sys/types.h>

#include <pinktrace/bitness.h>

/**
 * Get the data in argument arg and place it in dest.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param arg The number of the argument (0-5)
 * \param dest Pointer to store the data
 * \param len Length of the data
 **/
bool
pink_decode_simple(pid_t pid, pink_bitness_t bitness, int arg, void *dest, size_t len);

/**
 * Get the string argument and place it in dest.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param arg The number of the argument (0-5)
 * \param dest Pointer to store the string
 * \param len Length of the string
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_string(pid_t pid, pink_bitness_t bitness, int arg, char *dest, size_t len);

/**
 * Like pink_decode_string() but allocates the string itself.
 *
 * \return The path on success, NULL on failure and sets errno
 * accordingly.
 **/
char *
pink_decode_string_persistent(pid_t pid, pink_bitness_t bitness, int arg);

#endif /* !PINKTRACE_GUARD_DECODE_H */
