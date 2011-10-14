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
pinkpy_trace_me(PINK_GCC_ATTR((unused)) PyObject *self, PINK_GCC_ATTR((unused)) PyObject *args)
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
	"@param addr: On FreeBSD this argument is an address specifying the place\n"
	"where execution is to be resumed (a new value for program counter), or C{1}\n"
	"to indicate that execution is to pick up where it left off.\n"
	"On Linux, this argument is not used.\n"
	"(Optional, defaults to C{1})\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.";
static PyObject *
pinkpy_trace_cont(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	int sig;
	long addr;

	sig = 0;
	addr = 1;
	if (!PyArg_ParseTuple(args, PARSE_PID"|il", &pid, &sig, &addr))
		return NULL;

	if (!pink_trace_cont(pid, sig, (char *)addr))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_resume_doc[] = ""
	"Resumes the stopped child process.\n"
	"This is equivalent to C{pinktrace.trace.cont(pid, sig, 1)}.\n"
	"\n"
	"@param pid: Process ID of the child to be restarted\n"
	"@param sig: If this is non-zero and not B{SIGSTOP}, it is interpreted as the\n"
	"signal to be delivered to the child; otherwise, no signal is delivered.\n"
	"Thus, for example, the parent can control whether a signal sent to the child\n"
	"is delivered or not. (Optional, defaults to C{0})\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.";
static PyObject *
pinkpy_trace_resume(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_resume(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
}

static char pinkpy_trace_kill_doc[] = ""
	"Kills the traced child process with B{SIGKILL}.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.";
static PyObject *
pinkpy_trace_kill(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
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
pinkpy_trace_singlestep(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
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
pinkpy_trace_syscall(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
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

static char pinkpy_trace_syscall_entry_doc[] = ""
	"Restarts the stopped child process and arranges it to be stopped after the\n"
	"entry of the next system call.\n"
	"\n"
	"@note: Availability: FreeBSD\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param sig: Treated the same as the signal argument of C{pinktrace.trace.cont()}\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@see: pinktrace.trace.cont";
static PyObject *
pinkpy_trace_syscall_entry(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_FREEBSD)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_FREEBSD)
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_syscall_entry(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_FREEBSD) */
}

static char pinkpy_trace_syscall_exit_doc[] = ""
	"Restarts the stopped child process and arranges it to be stopped after the\n"
	"exit of the next system call.\n"
	"\n"
	"@note: Availability: FreeBSD\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param sig: Treated the same as the signal argument of C{pinktrace.trace.cont()}\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@see: pinktrace.trace.cont";
static PyObject *
pinkpy_trace_syscall_exit(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_FREEBSD)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_FREEBSD)
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_syscall_exit(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_FREEBSD) */
}

static char pinkpy_trace_sysemu_doc[] = ""
	"Restarts the stopped child process and arranges it to be stopped after\n"
	"the entry of the next system call which will *not* be executed.\n"
	"\n"
	"@note: Availability: Linux\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param sig: Treated the same as the signal argument of C{pinktrace.trace.cont()}\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@see: pinktrace.trace.cont";
static PyObject *
pinkpy_trace_sysemu(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_sysemu(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char pinkpy_trace_sysemu_singlestep_doc[] = ""
	"Restarts the stopped child process like C{pinktrace.trace.sysemu()}\n"
	"but also singlesteps if not a system call.\n"
	"\n"
	"@note: Availability: Linux\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param sig: Treated the same as the signal argument of C{pinktrace.trace.cont()}\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@see: pinktrace.trace.cont";
static PyObject *
pinkpy_trace_sysemu_singlestep(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	int sig;

	sig = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &sig))
		return NULL;

	if (!pink_trace_sysemu_singlestep(pid, sig))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char pinkpy_trace_geteventmsg_doc[] = ""
	"Retrieve a message (as an I{unsigned long}) about the trace event that just\n"
	"happened, placing it in the location given by the second argument. For B{EXIT}\n"
	"event this is the child's exit status. For B{FORK}, B{VFORK}, B{CLONE} and B{VFORK_DONE}\n"
	"events this is the process ID of the new process.\n"
	"\n"
	"@note: Availability: Linux\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: long\n"
	"@return: The event message";
static PyObject *
pinkpy_trace_geteventmsg(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	unsigned long data;

	if (!PyArg_ParseTuple(args, PARSE_PID, &pid))
		return NULL;

	if (!pink_trace_geteventmsg(pid, &data))
		return PyErr_SetFromErrno(PyExc_OSError);

	return PyLong_FromUnsignedLong(data);
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char pinkpy_trace_setup_doc[] = ""
	"Sets the tracing options.\n"
	"\n"
	"@note: Availability: Linux\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param options: Bitwise OR'ed C{pinktrace.trace.OPTION_*} flags\n"
	"(Optional, defaults to 0)\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n";
static PyObject *
pinkpy_trace_setup(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	int opts;

	opts = 0;
	if (!PyArg_ParseTuple(args, PARSE_PID"|i", &pid, &opts))
		return NULL;

	if (!pink_trace_setup(pid, opts))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("");
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
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
pinkpy_trace_attach(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
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
pinkpy_trace_detach(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
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
trace_init(
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *mod)
{
#if defined(PINKTRACE_LINUX)
	PyModule_AddIntConstant(mod, "OPTION_SYSGOOD", PINK_TRACE_OPTION_SYSGOOD);
	PyModule_AddIntConstant(mod, "OPTION_FORK", PINK_TRACE_OPTION_FORK);
	PyModule_AddIntConstant(mod, "OPTION_VFORK", PINK_TRACE_OPTION_VFORK);
	PyModule_AddIntConstant(mod, "OPTION_CLONE", PINK_TRACE_OPTION_CLONE);
	PyModule_AddIntConstant(mod, "OPTION_EXEC", PINK_TRACE_OPTION_EXEC);
	PyModule_AddIntConstant(mod, "OPTION_VFORK_DONE", PINK_TRACE_OPTION_VFORK_DONE);
	PyModule_AddIntConstant(mod, "OPTION_EXIT", PINK_TRACE_OPTION_EXIT);
	PyModule_AddIntConstant(mod, "OPTION_ALL", PINK_TRACE_OPTION_ALL);
#endif /* defined(PINKTRACE_LINUX) */
}

static char trace_doc[] = "Pink's low level wrappers around I{ptrace(2)} internals";
static PyMethodDef trace_methods[] = {
	{"me", pinkpy_trace_me, METH_NOARGS, pinkpy_trace_me_doc},
	{"cont", pinkpy_trace_cont, METH_VARARGS, pinkpy_trace_cont_doc},
	{"resume", pinkpy_trace_resume, METH_VARARGS, pinkpy_trace_resume_doc},
	{"kill", pinkpy_trace_kill, METH_VARARGS, pinkpy_trace_kill_doc},
	{"singlestep", pinkpy_trace_singlestep, METH_VARARGS, pinkpy_trace_singlestep_doc},
	{"syscall", pinkpy_trace_syscall, METH_VARARGS, pinkpy_trace_syscall_doc},
	{"syscall_entry", pinkpy_trace_syscall_entry, METH_VARARGS, pinkpy_trace_syscall_entry_doc},
	{"syscall_exit", pinkpy_trace_syscall_exit, METH_VARARGS, pinkpy_trace_syscall_exit_doc},
	{"sysemu", pinkpy_trace_sysemu, METH_VARARGS, pinkpy_trace_sysemu_doc},
	{"sysemu_singlestep", pinkpy_trace_sysemu_singlestep, METH_VARARGS, pinkpy_trace_sysemu_singlestep_doc},
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
