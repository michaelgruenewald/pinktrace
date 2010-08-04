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
pinkpy_bitness_get(pink_unused PyObject *self, PyObject *args)
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
pinkpy_bitness_name(pink_unused PyObject *self, PyObject *args)
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

static char bitness_doc[] = "Pink's bitness modes";
static PyMethodDef bitness_methods[] = {
	{"get", pinkpy_bitness_get, METH_VARARGS, pinkpy_bitness_get_doc},
	{"name", pinkpy_bitness_name, METH_VARARGS, pinkpy_bitness_name_doc},
	{NULL, NULL, 0, NULL}
};

static void
bitness_init(PyObject *mod)
{
	PyModule_AddIntConstant(mod, "BITNESS_32", PINK_BITNESS_32);
	PyModule_AddIntConstant(mod, "BITNESS_64", PINK_BITNESS_64);
	PyModule_AddIntConstant(mod, "DEFAULT", PINKTRACE_DEFAULT_BITNESS);
	PyModule_AddIntConstant(mod, "SUPPORTED", PINKTRACE_SUPPORTED_BITNESS);
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
