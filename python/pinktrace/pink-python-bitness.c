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
PyInit_bitness(void);
#else
initbitness(void);
#endif /* PY_MAJOR_VERSION > 2 */

static char pinkpy_bitness_get_doc[] = ""
	"Returns the bitness of the given Process ID.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: int\n"
	"@return: One of C{pinktrace.bitness.BITNESS_*} constants";
static PyObject *
pinkpy_bitness_get(PINK_UNUSED PyObject *self, PyObject *args)
{
	pid_t pid;
	pink_bitness_t bit;

	if (!PyArg_ParseTuple(args, PARSE_PID, &pid))
		return NULL;

	bit = pink_bitness_get(pid);
	if (bit == PINK_BITNESS_UNKNOWN)
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("I", bit);
}

static char pinkpy_bitness_name_doc[] = ""
	"Returns the name of the given bitness.\n"
	"\n"
	"@param bitness: The bitness\n"
	"@rtype: str\n"
	"@return: The name of the bitness";
static PyObject *
pinkpy_bitness_name(PINK_UNUSED PyObject *self, PyObject *args)
{
	const char *strbit;
	pink_bitness_t bit;

	if (!PyArg_ParseTuple(args, "i", &bit))
		return NULL;

	strbit = pink_bitness_name(bit);

#if PY_MAJOR_VERSION > 2
	return PyUnicode_FromString(strbit);
#else
	return PyString_FromString(strbit);
#endif /* PY_MAJOR_VERSION > 2 */
}

static char pinkpy_bitness_wordsize_doc[] = ""
	"Returns the word size of the given bitness.\n"
	"\n"
	"@param bitness: The bitness\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@rtype: int\n"
	"@return: The word size of the bitness";
static PyObject *
pinkpy_bitness_wordsize(PINK_UNUSED PyObject *self, PyObject *args)
{
	unsigned short wordsize;
	pink_bitness_t bit;

	if (!PyArg_ParseTuple(args, "i", &bit))
		return NULL;
	if (!check_bitness(bit))
		return NULL;

	wordsize = pink_bitness_wordsize(bit);

	return Py_BuildValue("I", wordsize);
}

static char bitness_doc[] = "Pink's bitness modes";
static PyMethodDef bitness_methods[] = {
	{"get", pinkpy_bitness_get, METH_VARARGS, pinkpy_bitness_get_doc},
	{"name", pinkpy_bitness_name, METH_VARARGS, pinkpy_bitness_name_doc},
	{"wordsize", pinkpy_bitness_wordsize, METH_VARARGS, pinkpy_bitness_wordsize_doc},
	{NULL, NULL, 0, NULL}
};

static void
bitness_init(PyObject *mod)
{
	PyModule_AddIntConstant(mod, "BITNESS_32", PINK_BITNESS_32);
	PyModule_AddIntConstant(mod, "BITNESS_64", PINK_BITNESS_64);
	PyModule_AddIntConstant(mod, "DEFAULT", PINKTRACE_BITNESS_DEFAULT);
	PyModule_AddIntConstant(mod, "COUNT_SUPPORTED", PINKTRACE_BITNESS_COUNT_SUPPORTED);
	PyModule_AddIntConstant(mod, "BITNESS_32_SUPPORTED", PINKTRACE_BITNESS_32_SUPPORTED);
	PyModule_AddIntConstant(mod, "BITNESS_64_SUPPORTED", PINKTRACE_BITNESS_64_SUPPORTED);
}

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef bitness_module = {
	PyModuleDef_HEAD_INIT,
	"bitness",
	bitness_doc,
	-1,
	bitness_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_bitness(void)
{
	PyObject *mod;

	mod = PyModule_Create(&bitness_module);
	if (!mod)
		return NULL;

	bitness_init(mod);

	return mod;
}
#else
PyMODINIT_FUNC
initbitness(void)
{
	PyObject *mod;

	mod = Py_InitModule3("bitness", bitness_methods, bitness_doc);
	if (!mod)
		return;

	bitness_init(mod);
}
#endif /* PY_MAJOR_VERSION > 2 */
