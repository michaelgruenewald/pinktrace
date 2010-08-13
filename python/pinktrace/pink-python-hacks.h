/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon Python-2.6.4's Modules/posixmodule.c which is:
 *   Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
 *   Python Software Foundation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
#if !PINKTRACE_BITNESS_64_SUPPORTED
		PyErr_SetString(PyExc_ValueError, "Unsupported bitness");
		return false;
#endif
		break;
	case PINK_BITNESS_32:
#if !PINKTRACE_BITNESS_32_SUPPORTED
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
