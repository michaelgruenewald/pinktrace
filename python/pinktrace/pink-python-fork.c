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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <Python.h>
#include <pinktrace/pink.h>

#include "pink-python-hacks.h"

PyMODINIT_FUNC
initfork(void);

static char pinkpy_fork_doc[] = ""
	"fork(2) wrapper that sets up the child for tracing.\n"
	"\n"
	"@param options: Bitwise OR'ed C{pinktrace.trace.OPTION_*} flags\n"
	"(Optional, defaults to C{pinktrace.trace.OPTION_SYSGOOD})\n"
	"@raise OSError: Raised when the underlying fork or ptrace calls fail.\n"
	"@rtype: long\n"
	"@return: Return 0 in the child and the child's process ID in the parent.";
static PyObject *
pinkpy_fork(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	int opts;
	pink_error_t error;

	opts = PINK_TRACE_OPTION_SYSGOOD;
	if (!PyArg_ParseTuple(args, "|i", &opts))
		return NULL;

	pid = pink_fork(opts, &error);
	if (pid < 0) {
		/* TODO: use pink_error_tostring() */
		return PyErr_SetFromErrno(PyExc_OSError);
	}

	return PyLong_FromPid(pid);
}

static char fork_doc[] = "Pink's fork(2) wrapper";
static PyMethodDef methods[] = {
	{"fork", pinkpy_fork, METH_VARARGS, pinkpy_fork_doc},
	{NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initfork(void)
{
	PyObject *mod;
	mod = Py_InitModule3("fork", methods, fork_doc);
}
