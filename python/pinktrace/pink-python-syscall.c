/*
 * Copyright (c) 2010, 2011 Ali Polatel <alip@exherbo.org>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <Python.h>
#include <pinktrace/pink.h>

#include "pink-python-hacks.h"

PyMODINIT_FUNC
#if PY_MAJOR_VERSION > 2
PyInit_syscall(void);
#else
initsyscall(void);
#endif /* PY_MAJOR_VERSION > 2 */

static char pinkpy_syscall_name_doc[] = ""
	"Return the name of the given system call.\n"
	"\n"
	"@param scno: System call number\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@return: The name of the system call or C{None}";
static PyObject *
pinkpy_syscall_name(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	long scno;
	pink_bitness_t bit;
	const char *scname;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, "l|i", &scno, &bit))
		return NULL;

	if (!check_bitness(bit))
		return NULL;

	scname = pink_name_syscall(scno, bit);
	/* "s" returns None if scname is NULL */
	return Py_BuildValue("s", scname);
}

static char pinkpy_syscall_lookup_doc[] = ""
	"Look up the number of the given system call name.\n"
	"\n"
	"@param name: System call name\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@return: The number of the system call or -1.";
static PyObject *
pinkpy_syscall_lookup(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	long scno;
	pink_bitness_t bit;
	const char *name;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, "s|i", &name, &bit))
		return NULL;

	if (!check_bitness(bit))
		return NULL;

	scno = pink_name_lookup(name, bit);
	return Py_BuildValue("l", scno);
}

static char pinkpy_syscall_get_no_doc[] = ""
	"Returns the last system call number called by the traced child.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param bitness: Bitness of the traced child\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: long\n"
	"@return: The number of the system call";
static PyObject *
pinkpy_syscall_get_no(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	pink_bitness_t bit;
	long scno;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"|I", &pid, &bit))
		return NULL;

	if (!check_bitness(bit))
		return NULL;

	if (!pink_util_get_syscall(pid, bit, &scno))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", scno);
}

static char pinkpy_syscall_set_no_doc[] = ""
	"Sets the number of the last system call called by the traced child.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param scno: The number of the system call\n"
	"@param bitness: Bitness of the traced child\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n";
static PyObject *
pinkpy_syscall_set_no(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	pink_bitness_t bit;
	long scno;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"l|I", &pid, &scno, &bit))
		return NULL;

	if (!check_bitness(bit))
		return NULL;

	if (!pink_util_set_syscall(pid, bit, scno))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_syscall_get_reg_doc[] = ""
	"Returns the value of the register in the traced child.\n"
	"@param pid: Process ID of the traced child\n"
	"@param regnum: Number of the register\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: long\n"
	"@return: The register value";
static PyObject *
pinkpy_syscall_get_reg(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned int idx;
	unsigned long value;

	if (!PyArg_ParseTuple(args, PARSE_PID"I", &pid, &idx))
		return NULL;

	if (!pink_util_get_reg(pid, idx, &value))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("K", value);
}

static char pinkpy_syscall_set_reg_doc[] = ""
	"Sets the value of the register in the traced child.\n"
	"@param pid: Process ID of the traced child\n"
	"@param regnum: Number of the register\n"
	"@param value: The register value\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: None\n";
static PyObject *
pinkpy_syscall_set_reg(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned int idx;
	unsigned long value;

	if (!PyArg_ParseTuple(args, PARSE_PID"IK", &pid, &idx, &value))
		return NULL;

	if(!pink_util_set_reg(pid, idx, value))
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
pinkpy_syscall_get_ret(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
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
pinkpy_syscall_set_ret(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
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
pinkpy_syscall_get_arg(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	long arg;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"I|I", &pid, &ind, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	if (!pink_util_get_arg(pid, bit, ind, &arg))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", arg);
}

static char pinkpy_syscall_set_arg_doc[] = ""
	"Sets the system call argument at the specified index to the given value.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param arg: The new value of the argument\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than C{MAX_INDEX}\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n";
static PyObject *
pinkpy_syscall_set_arg(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	long arg;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"Il|I", &pid, &ind, &arg, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	if (!pink_util_set_arg(pid, bit, ind, arg))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static void
syscall_init(PyObject *mod)
{
	PyModule_AddIntConstant(mod, "INVALID", PINKTRACE_INVALID_SYSCALL);
	PyModule_AddIntConstant(mod, "MAX_INDEX", PINK_MAX_INDEX);
}

static char syscall_doc[] = "Pink's system call utility functions";
static PyMethodDef syscall_methods[] = {
	{"name", pinkpy_syscall_name, METH_VARARGS, pinkpy_syscall_name_doc},
	{"lookup", pinkpy_syscall_lookup, METH_VARARGS, pinkpy_syscall_lookup_doc},
	{"get_no", pinkpy_syscall_get_no, METH_VARARGS, pinkpy_syscall_get_no_doc},
	{"set_no", pinkpy_syscall_set_no, METH_VARARGS, pinkpy_syscall_set_no_doc},
	{"get_reg", pinkpy_syscall_get_reg, METH_VARARGS, pinkpy_syscall_get_reg_doc},
	{"set_reg", pinkpy_syscall_set_reg, METH_VARARGS, pinkpy_syscall_set_reg_doc},
	{"get_ret", pinkpy_syscall_get_ret, METH_VARARGS, pinkpy_syscall_get_ret_doc},
	{"set_ret", pinkpy_syscall_set_ret, METH_VARARGS, pinkpy_syscall_set_ret_doc},
	{"get_arg", pinkpy_syscall_get_arg, METH_VARARGS, pinkpy_syscall_get_arg_doc},
	{"set_arg", pinkpy_syscall_set_arg, METH_VARARGS, pinkpy_syscall_set_arg_doc},
	{NULL, NULL, 0, NULL}
};

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef syscall_module = {
	PyModuleDef_HEAD_INIT,
	"syscall",
	syscall_doc,
	-1,
	syscall_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_syscall(void)
{
	PyObject *mod;

	mod = PyModule_Create(&syscall_module);
	if (!mod)
		return NULL;

	syscall_init(mod);

	return mod;
}
#else
PyMODINIT_FUNC
initsyscall(void)
{
	PyObject *mod;

	mod = Py_InitModule3("syscall", syscall_methods, syscall_doc);
	if (!mod)
		return;

	syscall_init(mod);
}
#endif /* PY_MAJOR_VERSION > 2 */
