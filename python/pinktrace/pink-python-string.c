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

#include "pink-python-hacks.h"

PyMODINIT_FUNC
#if PY_MAJOR_VERSION > 2
PyInit_string(void);
#else
initstring(void);
#endif /* PY_MAJOR_VERSION > 2 */

static char pinkpy_string_decode_doc[] = ""
	"This function decodes the string at the argument of the given index.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param maxlen: Max length of the string\n"
	"(Optional, defaults to -1, if smaller than zero, pinktrace tries to determine the string length itself)\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than MAX_INDEX\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: str\n"
	"@return: The decoded string";
static PyObject *
pinkpy_string_decode(PINK_UNUSED PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	int maxlen;
	pink_bitness_t bit;
	char *str;
	PyObject *obj;

	maxlen = -1;
	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"I|iI", &pid, &ind, &maxlen, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	if (maxlen < 0) {
		str = pink_decode_string_persistent(pid, bit, ind);
		if (!str)
			return PyErr_SetFromErrno(PyExc_OSError);
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

	if (!pink_decode_string(pid, bit, ind, str, maxlen))
		return PyErr_SetFromErrno(PyExc_OSError);

#if PY_MAJOR_VERSION > 2
	obj = PyUnicode_FromStringAndSize(str, maxlen);
#else
	obj = PyString_FromStringAndSize(str, maxlen);
#endif /* PY_MAJOR_VERSION > 2 */
	PyMem_Free(str);
	return obj;
}

static char pinkpy_string_encode_doc[] = ""
	"Encode a string into the argument of the given index.\n"
	"@bug: Care must be taken when using this function as unexpected things may happen.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param string: The string to be encoded\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than C{MAX_INDEX}\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n";
static PyObject *
pinkpy_string_encode(PINK_UNUSED PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	char *str;
	Py_ssize_t len;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"Is#|I", &pid, &ind, &str, &len, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	if (!pink_encode_simple(pid, bit, ind, str, ++len))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_string_encode_safe_doc[] = ""
	"Encode a string into the argument of the given index with additional\n"
	"checking for writable areas.\n"
	"@bug: Care must be taken when using this function as unexpected things may happen.\n"
	"@note: Availability: Linux\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param string: The string to be encoded\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than C{MAX_INDEX}\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n";
static PyObject *
pinkpy_string_encode_safe(PINK_UNUSED PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	char *str;
	Py_ssize_t len;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"Is#|I", &pid, &ind, &str, &len, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	if (!pink_encode_simple_safe(pid, bit, ind, str, ++len))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char string_doc[] = "Pink's string decoding and encoding functions";
static PyMethodDef string_methods[] = {
	{"decode", pinkpy_string_decode, METH_VARARGS, pinkpy_string_decode_doc},
	{"encode", pinkpy_string_encode, METH_VARARGS, pinkpy_string_encode_doc},
	{"encode_safe", pinkpy_string_encode_safe, METH_VARARGS, pinkpy_string_encode_safe_doc},
	{NULL, NULL, 0, NULL}
};

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef string_module = {
	PyModuleDef_HEAD_INIT,
	"string",
	string_doc,
	-1,
	string_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_string(void)
{
	PyObject *mod;

	mod = PyModule_Create(&string_module);
	if (!mod)
		return NULL;

	return mod;
}
#else
PyMODINIT_FUNC
initstring(void)
{
	PyObject *mod;
	mod = Py_InitModule3("string", string_methods, string_doc);
}
#endif /* PY_MAJOR_VERSION > 2 */
