/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon Python-2.6.4's Modules/posixmodule.c which is:
 *   Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
 *   Python Software Foundation
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

#ifndef PINKTRACE_GUARD_PINK_HACKS_H
#define PINKTRACE_GUARD_PINK_HACKS_H 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <Python.h>
#include <pinktrace/pink.h>

#include <stdbool.h>

#if PY_MAJOR_VERSION < 3

#if !defined(SIZEOF_PID_T) || SIZEOF_PID_T == SIZEOF_INT
#define PARSE_PID "i"
#define PyLong_FromPid PyInt_FromLong
#define PyLong_AsPid PyInt_AsLong
#elif SIZEOF_PID_T == SIZEOF_LONG
#define PARSE_PID "l"
#define PyLong_FromPid PyInt_FromLong
#define PyLong_AsPid PyInt_AsLong
#elif defined(SIZEOF_LONG_LONG) && SIZEOF_PID_T == SIZEOF_LONG_LONG
#define PARSE_PID "L"
#define PyLong_FromPid PyLong_FromLongLong
#define PyLong_AsPid PyInt_AsLongLong
#else
#error "sizeof(pid_t) is neither sizeof(int), sizeof(long) or sizeof(long long)"
#endif /* SIZEOF_PID_T */

#else

#if !defined(SIZEOF_PID_T) || SIZEOF_PID_T == SIZEOF_INT
#define PARSE_PID "i"
#define PyLong_FromPid PyLong_FromLong
#define PyLong_AsPid PyLong_AsLong
#elif SIZEOF_PID_T == SIZEOF_LONG
#define PARSE_PID "l"
#define PyLong_FromPid PyLong_FromLong
#define PyLong_AsPid PyLong_AsLong
#elif defined(SIZEOF_LONG_LONG) && SIZEOF_PID_T == SIZEOF_LONG_LONG
#define PARSE_PID "L"
#define PyLong_FromPid PyLong_FromLongLong
#define PyLong_AsPid PyLong_AsLongLong
#else
#error "sizeof(pid_t) is neither sizeof(int), sizeof(long) or sizeof(long long)"
#endif /* SIZEOF_PID_T */

#endif /* PY_MAJOR_VERSION < 3 */

pink_unused
static bool
check_bitness(int bit)
{
	switch (bit) {
	case PINK_BITNESS_64:
#if defined(I386) || defined(POWERPC)
		PyErr_SetString(PyExc_ValueError, "Unsupported bitness");
		return false;
#endif
		break;
	case PINK_BITNESS_32:
#if defined(IA64) || defined(POWERPC64)
		PyErr_SetString(PyExc_ValueError, "Unsupported bitness");
		return false;
#endif
		break;
	default:
		PyErr_SetString(PyExc_ValueError, "Invalid bitness");
		return false;
	}

	return true;
}

pink_unused
static bool
check_index(unsigned ind)
{
	if (ind >= PINK_MAX_INDEX) {
		PyErr_SetString(PyExc_IndexError, "Invalid index");
		return false;
	}
	return true;
}

#endif /* !PINKTRACE_GUARD_PINK_HACKS_H */
