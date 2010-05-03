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

#include "pink-hacks.h"

PyMODINIT_FUNC
initsyscall(void);

static char pinkpy_syscall_name_doc[] = ""
	"Return the name of the given system call.\n"
	"\n"
	"Note: This call depends on the generated system call names.\n"
	"You can check whether they are generated with:\n"
	"C{pinktrace.syscall.name(0) is not None}\n"
	"@param scno: System call number\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@return: The name of the system call or C{None}";
static PyObject *
pinkpy_syscall_name(pink_unused PyObject *self, PyObject *args)
{
	long scno;
	pink_bitness_t bit;
	const char *scname;

	bit = PINKTRACE_DEFAULT_BITNESS;
	if (!PyArg_ParseTuple(args, "l|i", &scno, &bit))
		return NULL;

	switch (bit) {
	case PINK_BITNESS_64:
#if defined(I386) || defined(POWERPC)
		PyErr_SetString(PyExc_ValueError, "Unsupported bitness");
		return NULL;
#endif
		break;
	case PINK_BITNESS_32:
#if defined(IA64) || defined(POWERPC64)
		PyErr_SetString(PyExc_ValueError, "Unsupported bitness");
		return NULL;
#endif
		break;
	default:
		PyErr_SetString(PyExc_ValueError, "Invalid bitness");
		return NULL;
	}

	scname = pink_name_syscall(scno, bit);
	/* "s" returns None if scname is NULL */
	return Py_BuildValue("s", scname);
}

static char pinkpy_syscall_get_no_doc[] = ""
	"Returns the last system call number called by the traced child.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: long\n"
	"@return: The number of the system call";
static PyObject *
pinkpy_syscall_get_no(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	long scno;

	if (!PyArg_ParseTuple(args, PARSE_PID, &pid))
		return NULL;

	if (!pink_util_get_syscall(pid, &scno))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", scno);
}

static char pinkpy_syscall_set_no_doc[] = ""
	"Sets the number of the last system call called by the traced child.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param scno: The number of the system call\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n";
static PyObject *
pinkpy_syscall_set_no(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	long scno;

	if (!PyArg_ParseTuple(args, PARSE_PID"l", &pid, &scno))
		return NULL;

	if (!pink_util_set_syscall(pid, scno))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_syscall_get_ret_doc[] = ""
	"Returns the return value of the last system call called by the traced child.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: long\n"
	"@return: The return value";
static PyObject *
pinkpy_syscall_get_ret(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	long ret;

	if (!PyArg_ParseTuple(args, PARSE_PID, &pid))
		return NULL;

	if (!pink_util_get_return(pid, &ret))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", ret);
}


static char pinkpy_syscall_set_ret_doc[] = ""
	"Sets the return value of the last system call called by the traced child.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param ret: The return value\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n";
static PyObject *
pinkpy_syscall_set_ret(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	long ret;

	if (!PyArg_ParseTuple(args, PARSE_PID"l", &pid, &ret))
		return NULL;

	if (!pink_util_set_return(pid, ret))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_syscall_get_arg_doc[] = ""
	"Returns the system call argument at the given index.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than C{MAX_INDEX}\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: long\n"
	"@return: The value of the argument";
static PyObject *
pinkpy_syscall_get_arg(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	long arg;

	bit = PINKTRACE_DEFAULT_BITNESS;
	if (!PyArg_ParseTuple(args, PARSE_PID"I|I", &pid, &ind, &bit))
		return NULL;

	switch (bit) {
	case PINK_BITNESS_64:
#if defined(I386) || defined(POWERPC)
		PyErr_SetString(PyExc_ValueError, "Unsupported bitness");
		return NULL;
#endif
		break;
	case PINK_BITNESS_32:
#if defined(IA64) || defined(POWERPC64)
		PyErr_SetString(PyExc_ValueError, "Unsupported bitness");
		return NULL;
#endif
		break;
	default:
		PyErr_SetString(PyExc_ValueError, "Invalid bitness");
		return NULL;
	}

	if (ind >= PINK_MAX_INDEX) {
		PyErr_SetString(PyExc_IndexError, "Invalid index");
		return NULL;
	}

	if (!pink_util_get_arg(pid, bit, ind, &arg))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", arg);
}

static char syscall_doc[] = "Pink's system call utility functions";
static PyMethodDef methods[] = {
	{"name", pinkpy_syscall_name, METH_VARARGS, pinkpy_syscall_name_doc},
	{"get_no", pinkpy_syscall_get_no, METH_VARARGS, pinkpy_syscall_get_no_doc},
	{"set_no", pinkpy_syscall_set_no, METH_VARARGS, pinkpy_syscall_set_no_doc},
	{"get_ret", pinkpy_syscall_get_ret, METH_VARARGS, pinkpy_syscall_get_ret_doc},
	{"set_ret", pinkpy_syscall_set_ret, METH_VARARGS, pinkpy_syscall_set_ret_doc},
	{"get_arg", pinkpy_syscall_get_arg, METH_VARARGS, pinkpy_syscall_get_arg_doc},
	{NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initsyscall(void)
{
	PyObject *mod;

	mod = Py_InitModule3("syscall", methods, syscall_doc);
	if (!mod)
		return;

	PyModule_AddIntConstant(mod, "MAX_INDEX", PINK_MAX_INDEX);
}
