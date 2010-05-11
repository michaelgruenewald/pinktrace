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

#ifndef PINKTRACE_GUARD_NAME_H
#define PINKTRACE_GUARD_NAME_H 1

/**
 * \file
 * Pink's system call naming
 **/

#include <pinktrace/bitness.h>

/**
 * Return the name of the given system call.
 *
 * \param scno System call number
 * \param bitness Bitness of the child
 *
 * \return The name of the system call, NULL if system call name is unknown.
 **/
const char *
pink_name_syscall(long scno, pink_bitness_t bitness);

/**
 * Look up the number of the given system call name.
 *
 * \param name Name of the system call
 * \param bitness Bitness of the child
 *
 * \return The system call number on success, -1 on failure.
 **/
long
pink_name_lookup(const char *name, pink_bitness_t bitness);

#endif /* !PINKTRACE_GUARD_NAME_H */
