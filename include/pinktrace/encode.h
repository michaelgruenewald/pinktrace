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

#ifndef PINKTRACE_GUARD_ENCODE_H
#define PINKTRACE_GUARD_ENCODE_H 1

/**
 * \file
 * Pink's system call encoders
 **/

#include <stdbool.h>
#include <sys/types.h>

#include <pinktrace/bitness.h>

/**
 * Write the given data argument src to the address of the argument arg.
 *
 * \param pid Process ID of the child whose argument is to be set.
 * \param bitness Bitness of the child
 * \param ind The index of the argument (0-5, see #PINK_MAX_INDEX)
 * \param src The data to be encoded
 * \param len Length of the data
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_encode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, const void *src, size_t len);

#if defined(PINKTRACE_LINUX) || defined(DOXYGEN)
/**
 * Write the given data argument src to the address of the argument arg safely.
 *
 * \note Availability: Linux
 *
 * \param pid Process ID of the child whose argument is to be set.
 * \param bitness Bitness of the child
 * \param ind The index of the argument (0-5, see #PINK_MAX_INDEX)
 * \param src The data to be encoded
 * \param len Length of the data
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_encode_simple_safe(pid_t pid, pink_bitness_t bitness, unsigned ind, const void *src, size_t len);
#endif /* defined(PINKTRACE_LINUX)... */

#endif /* !PINKTRACE_GUARD_ENCODE_H */
