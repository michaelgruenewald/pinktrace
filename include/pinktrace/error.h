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

#ifndef PINKTRACE_GUARD_ERROR_H
#define PINKTRACE_GUARD_ERROR_H 1

/**
 * \file
 * Pink's error constants
 **/

/**
 * Error constants
 **/
typedef enum {
	/** Success **/
	PINK_ERROR_SUCCESS = 0,
	/** Fork failed **/
	PINK_ERROR_FORK,
	/** pink_trace_me() failed **/
	PINK_ERROR_TRACE,
	/** pink_trace_setup() failed **/
	PINK_ERROR_TRACE_SETUP,
	/** Unknown error **/
	PINK_ERROR_UNKNOWN,
} pink_error_t;

/**
 * Return a string representation of the error
 *
 * \param error The error to be stringified.
 *
 * \return The string representation of the error.
 **/
const char *
pink_error_tostring(pink_error_t error);

#endif /* !PINKTRACE_GUARD_ERROR_H */
