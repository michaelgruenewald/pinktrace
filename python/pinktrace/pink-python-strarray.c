/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
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

#include <errno.h>
#include "pink-python-hacks.h"

PyMODINIT_FUNC
#if PY_MAJOR_VERSION > 2
PyInit_strarray(void);
#else
initstrarray(void);
#endif /* PY_MAJOR_VERSION > 2 */

static char pinkpy_strarray_decode_doc[] = ""
	"This function decodes the member of the string array at the given index.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param arg: Address of the argument, see C{pinktrace.syscall.get_arg()}\n"
	"@param index: The index of the member\n"
	"@param maxlen: Max length of the string\n"
	"(Optional, defaults to -1, if smaller than zero, pinktrace tries to determine the string length itself)\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: str\n"
	"@return: The decoded string, C{None} if the member is NULL.";
static PyObject *
pinkpy_strarray_decode(PINK_UNUSED PyObject *self, PyObject *args)
{
	bool nil;
	pid_t pid;
	unsigned ind;
	long arg;
	int maxlen;
	pink_bitness_t bit;
	char *str;
	PyObject *obj;

	maxlen = -1;
	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"lI|iI", &pid, &arg, &ind, &maxlen, &bit))
		return NULL;

	if (!check_bitness(bit))
		return NULL;

	if (maxlen < 0) {
		errno = 0;
		str = pink_decode_string_array_member_persistent(pid, bit, arg, ind);
		if (!str) {
			if (errno)
				return PyErr_SetFromErrno(PyExc_OSError);
			return Py_BuildValue("");
		}
#if PY_MAJOR_VERSION > 2
		obj = PyUnicode_FromString(str);
#else
		obj = PyString_FromString(str);
#endif /* PY_MAJOR_VERSION > 2 */
		free(str);
		return obj;
	}

	str = PyMem_Malloc(sizeof(char) * maxlen);
	if (!str)
		return PyErr_NoMemory();

	if (!pink_decode_string_array_member(pid, bit, arg, ind, str, maxlen, &nil))
		return PyErr_SetFromErrno(PyExc_OSError);
	if (nil) {
		PyMem_Free(str);
		return Py_BuildValue("");
	}

#if PY_MAJOR_VERSION > 2
	obj = PyUnicode_FromStringAndSize(str, maxlen);
#else
	obj = PyString_FromStringAndSize(str, maxlen);
#endif /* PY_MAJOR_VERSION > 2 */
	PyMem_Free(str);
	return obj;
}

static char strarray_doc[] = "Pink's string array decoding functions";
static PyMethodDef strarray_methods[] = {
	{"decode", pinkpy_strarray_decode, METH_VARARGS, pinkpy_strarray_decode_doc},
	{NULL, NULL, 0, NULL}
};

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef strarray_module = {
	PyModuleDef_HEAD_INIT,
	"strarray",
	strarray_doc,
	-1,
	strarray_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_strarray(void)
{
	PyObject *mod;

	mod = PyModule_Create(&strarray_module);
	if (!mod)
		return NULL;

	return mod;
}
#else
PyMODINIT_FUNC
initstrarray(void)
{
	PyObject *mod;
	mod = Py_InitModule3("strarray", strarray_methods, strarray_doc);
}
#endif /* PY_MAJOR_VERSION > 2 */
