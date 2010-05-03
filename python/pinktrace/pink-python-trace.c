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
PyInit_trace(void);
#else
inittrace(void);
#endif /* PY_MAJOR_VERSION */

static char pinkpy_trace_me_doc[] = ""
	"Indicates that this process is to be traced by its parent.\n"
	"Any signal (except B{SIGKILL}) delivered to this process will cause it to stop\n"
	"and its parent to be notified via I{wait(2)}. Also, all subsequent calls to\n"
	"I{execve(2)} by this process will cause a B{SIGTRAP} to be sent to it, giving the\n"
	"parent a chance to gain control before the new program begins execution.\n"
	"\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.";
static PyObject *
pinkpy_trace_me(pink_unused PyObject *self, pink_unused PyObject *args)
{
	if (!pink_trace_me())
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_cont_doc[] = ""
	"Restarts the stopped child process.\n"
	"\n"
	"@param pid: Process ID of the child to be restarted\n"
	"@param sig: If this is non-zero and not B{SIGSTOP}, it is interpreted as the\n"
	"signal to be delivered to the child; otherwise, no signal is delivered.\n"
	"Thus, for example, the parent can control whether a signal sent to the child\n"
	"is delivered or not. (Optional, defaults to C{0})\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.";
static PyObject *
pinkpy_trace_cont(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_cont(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_kill_doc[] = ""
	"Kills the traced child process with B{SIGKILL}.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.";
static PyObject *
pinkpy_trace_kill(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;

	if (!PyArg_ParseTuple(args, PARSE_PID, &pid))
		return NULL;

	if (!pink_trace_kill(pid))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_singlestep_doc[] = ""
	"Restarts the stopped child process and arranges it to be stopped after\n"
	"execution of a single instruction.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param sig: Treated the same as the signal argument of C{pinktrace.trace.cont()}\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@see: pinktrace.trace.cont";
static PyObject *
pinkpy_trace_singlestep(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_singlestep(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_syscall_doc[] = ""
	"Restarts the stopped child process and arranges it to be stopped after the\n"
	"entry or exit of the next system call.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param sig: Treated the same as the signal argument of C{pinktrace.trace.cont()}\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@see: pinktrace.trace.cont";
static PyObject *
pinkpy_trace_syscall(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_syscall(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_geteventmsg_doc[] = ""
	"Retrieve a message (as an I{unsigned long}) about the trace event that just\n"
	"happened, placing it in the location given by the second argument. For B{EXIT}\n"
	"event this is the child's exit status. For B{FORK}, B{VFORK}, B{CLONE} and B{VFORK_DONE}\n"
	"events this is the process ID of the new process.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: long\n"
	"@return: The event message";
static PyObject *
pinkpy_trace_geteventmsg(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned long data;

	if (!PyArg_ParseTuple(args, PARSE_PID, &pid))
		return NULL;

	if (!pink_trace_geteventmsg(pid, &data))
		return PyErr_SetFromErrno(PyExc_OSError);

	return PyLong_FromUnsignedLong(data);
}

static char pinkpy_trace_setup_doc[] = ""
	"Sets the tracing options.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param options: Bitwise OR'ed C{pinktrace.trace.OPTION_*} flags\n"
	"(Optional, defaults to C{pinktrace.trace.OPTION_SYSGOOD})\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n";
static PyObject *
pinkpy_trace_setup(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	int opts;

	opts = PINK_TRACE_OPTION_SYSGOOD;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &opts))
		return NULL;

	if (!pink_trace_setup(pid, opts))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_attach_doc[] = ""
	"Attaches to the process specified in pid, making it a traced \"child\" of the\n"
	"calling process; the behaviour of the child is as if it had done a\n"
	"C{pinktrace.trace.me()}. The child is sent a B{SIGSTOP}, but will not necessarily\n"
	"have stopped by the completion of this call; use I{wait(2)} to wait for the\n"
	"child to stop.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n";
static PyObject *
pinkpy_trace_attach(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;

	if (!PyArg_ParseTuple(args, PARSE_PID, &pid))
		return NULL;

	if (!pink_trace_attach(pid))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_detach_doc[] = ""
	"Restarts the stopped child as for C{pinktrace.trace.cont()}, but first detaches\n"
	"from the process, undoing the reparenting effect of\n"
	"C{pinktrace.trace.attach()}.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param sig: Treated the same as the signal argument of pinktrace.trace.cont()\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@see: pinktrace.trace.cont";
static PyObject *
pinkpy_trace_detach(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_detach(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static void
trace_init(PyObject *mod)
{
	PyModule_AddIntConstant(mod, "OPTION_SYSGOOD", PINK_TRACE_OPTION_SYSGOOD);
	PyModule_AddIntConstant(mod, "OPTION_FORK", PINK_TRACE_OPTION_FORK);
	PyModule_AddIntConstant(mod, "OPTION_VFORK", PINK_TRACE_OPTION_VFORK);
	PyModule_AddIntConstant(mod, "OPTION_CLONE", PINK_TRACE_OPTION_CLONE);
	PyModule_AddIntConstant(mod, "OPTION_EXEC", PINK_TRACE_OPTION_EXEC);
	PyModule_AddIntConstant(mod, "OPTION_VFORK_DONE", PINK_TRACE_OPTION_VFORK_DONE);
	PyModule_AddIntConstant(mod, "OPTION_EXIT", PINK_TRACE_OPTION_EXIT);
	PyModule_AddIntConstant(mod, "OPTION_ALL", PINK_TRACE_OPTION_ALL);
}

static char trace_doc[] = "Pink's low level wrappers around ptrace internals";
static PyMethodDef trace_methods[] = {
	{"me", pinkpy_trace_me, METH_NOARGS, pinkpy_trace_me_doc},
	{"cont", pinkpy_trace_cont, METH_VARARGS, pinkpy_trace_cont_doc},
	{"kill", pinkpy_trace_kill, METH_VARARGS, pinkpy_trace_kill_doc},
	{"singlestep", pinkpy_trace_singlestep, METH_VARARGS, pinkpy_trace_singlestep_doc},
	{"syscall", pinkpy_trace_syscall, METH_VARARGS, pinkpy_trace_syscall_doc},
	{"geteventmsg", pinkpy_trace_geteventmsg, METH_VARARGS, pinkpy_trace_geteventmsg_doc},
	{"setup", pinkpy_trace_setup, METH_VARARGS, pinkpy_trace_setup_doc},
	{"attach", pinkpy_trace_attach, METH_VARARGS, pinkpy_trace_attach_doc},
	{"detach", pinkpy_trace_detach, METH_VARARGS, pinkpy_trace_detach_doc},
	{NULL, NULL, 0, NULL},
};

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef trace_module = {
	PyModuleDef_HEAD_INIT,
	"trace",
	trace_doc,
	-1,
	trace_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_trace(void)
{
	PyObject *mod;

	mod = PyModule_Create(&trace_module);
	if (!mod)
		return NULL;

	trace_init(mod);

	return mod;
}
#else
PyMODINIT_FUNC
inittrace(void)
{
	PyObject *mod;

	mod = Py_InitModule3("trace", trace_methods, trace_doc);
	if (!mod)
		return;

	trace_init(mod);
}
#endif /* PY_MAJOR_VERSION > 2 */
