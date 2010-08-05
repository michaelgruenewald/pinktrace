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

PyMODINIT_FUNC
#if PY_MAJOR_VERSION > 2
PyInit_event(void);
#else
initevent(void);
#endif /* PY_MAJOR_VERSION > 2 */

static PyObject *EventError;

static char pinkpy_event_decide_doc[] = ""
	"Return the last event made by child.\n"
	"\n"
	"@note: Availability: Linux\n"
	"@note: This function expects C{pinktrace.trace.OPTION_SYSGOOD} has been passed\n"
	"to C{pinktrace.trace.setup()}\n"
	"\n"
	"@param status: The status argument, received from os.waitpid() call.\n"
	"@rtype: int\n"
	"@return: One of the C{pinktrace.event.EVENT_*} constants";
static PyObject *
pinkpy_event_decide(pink_unused PyObject *self,
#if !defined(PINKTRACE_LINUX)
	pink_unused
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	int status;
	pink_event_t event;

	if (!PyArg_ParseTuple(args, "i", &status))
		return NULL;

	event = pink_event_decide(status);
	return Py_BuildValue("I", event);
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char event_doc[] = "Pink's event handling";
static PyMethodDef event_methods[] = {
	{"decide", pinkpy_event_decide, METH_VARARGS, pinkpy_event_decide_doc},
	{NULL, NULL, 0, NULL}
};

static void
event_init(
#if !defined(PINKTRACE_LINUX)
	pink_unused
#endif
	PyObject *mod)
{
#if defined(PINKTRACE_LINUX)
	PyModule_AddIntConstant(mod, "EVENT_STOP", PINK_EVENT_STOP);
	PyModule_AddIntConstant(mod, "EVENT_SYSCALL", PINK_EVENT_SYSCALL);
	PyModule_AddIntConstant(mod, "EVENT_FORK", PINK_EVENT_FORK);
	PyModule_AddIntConstant(mod, "EVENT_VFORK", PINK_EVENT_VFORK);
	PyModule_AddIntConstant(mod, "EVENT_CLONE", PINK_EVENT_CLONE);
	PyModule_AddIntConstant(mod, "EVENT_EXEC", PINK_EVENT_EXEC);
	PyModule_AddIntConstant(mod, "EVENT_VFORK_DONE", PINK_EVENT_VFORK_DONE);
	PyModule_AddIntConstant(mod, "EVENT_EXIT", PINK_EVENT_EXIT);
	PyModule_AddIntConstant(mod, "EVENT_GENUINE", PINK_EVENT_GENUINE);
	PyModule_AddIntConstant(mod, "EVENT_EXIT_GENUINE", PINK_EVENT_EXIT_GENUINE);
	PyModule_AddIntConstant(mod, "EVENT_EXIT_SIGNAL", PINK_EVENT_EXIT_SIGNAL);
	PyModule_AddIntConstant(mod, "EVENT_UNKNOWN", PINK_EVENT_UNKNOWN);
#endif /* defined(PINKTRACE_LINUX) */
}

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef event_module = {
	PyModuleDef_HEAD_INIT,
	"event",
	event_doc,
	-1,
	event_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_event(void)
{
	PyObject *mod, *dict;

	mod = PyModule_Create(&event_module);
	if (!mod)
		return NULL;

	event_init(mod);

	dict = PyModule_GetDict(mod);
	if (!dict) {
		PyErr_SetString(PyExc_ImportError, "pinktrace.event: init failed");
		return NULL;
	}

	EventError = PyErr_NewException("event.EventError", PyExc_RuntimeError, NULL);
	if (!EventError)
		PyErr_SetString(PyExc_ImportError, "pinktrace.event: init failed");
	PyDict_SetItemString(dict, "EventError", EventError);

	return mod;
}
#else
PyMODINIT_FUNC
initevent(void)
{
	PyObject *mod, *dict;

	mod = Py_InitModule3("event", event_methods, event_doc);
	if (!mod)
		return;

	event_init(mod);

	dict = PyModule_GetDict(mod);
	if (!dict)
		PyErr_SetString(PyExc_ImportError, "pinktrace.event: init failed");

	EventError = PyErr_NewException("event.EventError", PyExc_RuntimeError, NULL);
	if (!EventError)
		PyErr_SetString(PyExc_ImportError, "pinktrace.event: init failed");
	PyDict_SetItemString(dict, "EventError", EventError);
}
#endif /* PY_MAJOR_VERSION > 2 */
