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

#ifndef PINKTRACE_GUARD_BITNESS_H
#define PINKTRACE_GUARD_BITNESS_H 1

#include <sys/types.h>

/**
 * \file
 * Pink's bitness modes
 **/

typedef enum {
	PINK_BITNESS_UNKNOWN = -1,	/*<< Unknown (error with pink_bitness_get() */
	PINK_BITNESS_32,		/*<< 32 bit mode */
	PINK_BITNESS_64,		/*<< 64 bit mode */
} pink_bitness_t;

/**
 * Return the bitness of the given process ID.
 *
 * \param pid Process ID of the process whose bitness is to be returned.
 *
 * \return One of PINK_BITNESS_* constants.
 **/
pink_bitness_t
pink_bitness_get(pid_t pid);

#endif /* !PINKTRACE_GUARD_BITNESS_H */
