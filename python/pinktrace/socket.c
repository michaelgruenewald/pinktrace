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

#include <arpa/inet.h> /* inet_ntop() */

#include "pink-hacks.h"

PyMODINIT_FUNC
initsocket(void);

typedef struct {
	PyObject_HEAD
	pink_socket_address_t addr;
} Address;

static char pinkpy_socket_name_doc[] = ""
	"Returns the name of the socket subcall.\n"
	"\n"
	"@param subcall: The socket subcall\n"
	"@rtype: str\n"
	"@return: The name of the socket subcall or C{None}";
static PyObject *
pinkpy_socket_name(pink_unused PyObject *self, PyObject *args)
{
	long subno;
	const char *subname;

	if (!PyArg_ParseTuple(args, "l", &subno))
		return NULL;

	subname = pink_name_socket_subcall(subno);
	/* "s" returns None if subname is NULL */
	return Py_BuildValue("s", subname);
}

static char pinkpy_socket_decode_call_doc[] = ""
	"Returns the decoded socket call.\n"
	"\n"
	"Note: This function decodes the socketcall(2) system call on some\n"
	"architectures. On others it's equivalent to pinktrace.syscall.get_no()\n"
	"\n"
	"@param pid: The process ID of the traced child\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: long\n"
	"@return: The decoded socket call";
static PyObject *
pinkpy_socket_decode_call(pink_unused PyObject *self, PyObject *args)
{
	long subcall;
	pid_t pid;
	pink_bitness_t bit;

	if (!PyArg_ParseTuple(args, PARSE_PID"|I", &pid, &bit))
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

	if (!pink_decode_socket_call(pid, bit, &subcall))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", subcall);
}

static char pinkpy_socket_decode_fd_doc[] = ""
	"Returns the socket file descriptor.\n"
	"\n"
	"Note: This function decodes the socketcall(2) system call on some\n"
	"architectures.\n"
	"\n"
	"@param index: The index of the argument\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than MAX_INDEX\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: long\n"
	"@return: The socket file descriptor";
static PyObject *
pinkpy_socket_decode_fd(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	long fd;

	ind = 0;
	bit = PINKTRACE_DEFAULT_BITNESS;
	if (!PyArg_ParseTuple(args, PARSE_PID"|II", &pid, &ind, &bit))
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

	if (!pink_decode_socket_fd(pid, bit, ind, &fd))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", fd);
}

static PyObject *
Address_new(PyTypeObject *t, pink_unused PyObject *a, pink_unused PyObject *k)
{
	return t->tp_alloc(t, 0);
}

static void
Address_dealloc(PyObject *o)
{
	o->ob_type->tp_free(o);
}

static PyObject *
Address_family(PyObject *self, pink_unused void *x)
{
	return PyInt_FromLong(((Address *)self)->addr.family);
}

static PyObject *
Address_path(PyObject *self, pink_unused void *x)
{
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_UNIX)
		return Py_BuildValue("s", addr->addr.u.sa_un.sun_path);
	return Py_BuildValue("");
}

static PyObject *
Address_ip(PyObject *self, pink_unused void *x)
{
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_INET) {
		char ip[INET_ADDRSTRLEN] = { 0 };
		inet_ntop(AF_INET, &addr->addr.u.sa_in.sin_addr, ip, INET_ADDRSTRLEN);
		return Py_BuildValue("s", ip);
	}
#if PINKTRACE_HAVE_IPV6
	else if (addr->addr.family == AF_INET6) {
		char ip6[INET6_ADDRSTRLEN] = { 0 };
		inet_ntop(AF_INET6, &addr->addr.u.sa6.sin6_addr, ip6, INET6_ADDRSTRLEN);
		return Py_BuildValue("s", ip6);
	}
#endif /* PINKTRACE_HAVE_IPV6 */

	return Py_BuildValue("");
}

static struct PyGetSetDef Address_get_sets[] = {
	{"family", Address_family, 0, 0, 0},
	{"path", Address_path, 0, 0, 0},
	{"ip", Address_ip, 0, 0, 0},
	{NULL, NULL, 0, 0, 0}
};

static PyTypeObject Address_type = {
	PyObject_HEAD_INIT(NULL)
	0,						/* ob_size */
	"socket.Address",				/* tp_name */
	sizeof(Address),				/* tp_basicsize */
	0,						/* tp_itemsize */
	(destructor)Address_dealloc,			/* tp_dealloc */
	0,						/* tp_print */
	0,						/* tp_getattr */
	0,						/* tp_setattr */
	0,						/* tp_compare */
	0,						/* tp_repr */
	0,						/* tp_as_number*/
	0,						/* tp_as_sequence*/
	0,						/* tp_as_mapping*/
	0,						/* tp_hash */
	0,						/* tp_call*/
	0,						/* tp_str*/
	0,						/* tp_getattro*/
	0,						/* tp_setattro*/
	0,						/* tp_as_buffer*/
	Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,	/* tp_flags*/
	"Decoded socket address",			/* tp_doc */
	0,						/* tp_traverse */
	0,						/* tp_clear */
	0,						/* tp_richcompare */
	0,						/* tp_weaklistoffset */
	0,						/* tp_iter */
	0,						/* tp_iternext */
	0,						/* tp_methods */
	0,						/* tp_members */
	Address_get_sets,				/* tp_getset */
	0,						/* tp_base */
	0,						/* tp_dict */
	0,						/* tp_descr_get */
	0,						/* tp_descr_set */
	0,						/* tp_dictoffset */
	0,						/* tp_init */
	0,						/* tp_alloc */
	Address_new,					/* tp_new */
	0,						/* tp_free */
	0,						/* tp_is_gc */
	0,						/* tp_bases */
	0,						/* tp_mro */
	0,						/* tp_cache */
	0,						/* tp_subclasses */
	0,						/* tp_weaklist */
	0,						/* tp_del */
	0,						/* tp_version_tag */
};

static char pinkpy_socket_decode_address_doc[] = ""
	"Decodes the socket address at the given argument index.\n"
	"\n"
	"Note: This function decodes the socketcall(2) system call on some\n"
	"architectures.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than C{MAX_INDEX}\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying ptrace call fails.\n"
	"@rtype: pinktrace.socket.Address\n"
	"@return: The decoded socket address";
static PyObject *
pinkpy_socket_decode_address(pink_unused PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	PyObject *obj;
	Address *addr;

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

	obj = PyObject_CallObject((PyObject *)&Address_type, NULL);
	if (!obj)
		return NULL;

	addr = (Address *)obj;
	if (!pink_decode_socket_address(pid, bit, ind, NULL, &addr->addr))
		return PyErr_SetFromErrno(PyExc_OSError);

	return obj;
}

static char socket_doc[] = "Pink's socket decoding functions";
static PyMethodDef methods[] = {
	{"name", pinkpy_socket_name, METH_VARARGS, pinkpy_socket_name_doc},
	{"decode_call", pinkpy_socket_decode_call, METH_VARARGS, pinkpy_socket_decode_call_doc},
	{"decode_fd", pinkpy_socket_decode_fd, METH_VARARGS, pinkpy_socket_decode_fd_doc},
	{"decode_address", pinkpy_socket_decode_address, METH_VARARGS, pinkpy_socket_decode_address_doc},
	{NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initsocket(void)
{
	PyObject *mod;

	if (PyType_Ready(&Address_type) < 0)
		return;

	mod = Py_InitModule3("socket", methods, socket_doc);

	Py_INCREF(&Address_type);
	PyModule_AddObject(mod, "Address", (PyObject *)&Address_type);
}
