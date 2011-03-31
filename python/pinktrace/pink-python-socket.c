/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

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

#include <stdbool.h>

#include <netinet/in.h> /* INET{,6}_ADDRSTRLEN */
#include <arpa/inet.h> /* inet_ntop() */

#include "pink-python-hacks.h"

#ifndef INET_ADDRSTRLEN
#define INET_ADDRSTRLEN 16
#endif /* !INET_ADDRSTRLEN */

#if PINKTRACE_HAVE_IPV6
#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif /* !INET6_ADDRSTRLEN */
#endif

#ifdef PINKTRACE_LINUX
#define IS_ABSTRACT(addr) \
	((addr).u.sa_un.sun_path[0] == '\0' \
	 && (addr).u.sa_un.sun_path[1] != '\0')
#else
#define IS_ABSTRACT(addr) 0
#endif /* PINKTRACE_LINUX */

PyMODINIT_FUNC
#if PY_MAJOR_VERSION > 2
PyInit_socket(void);
#else
initsocket(void);
#endif /* PY_MAJOR_VERSION */

typedef struct {
	PyObject_HEAD
	pink_socket_address_t addr;
} Address;

static char pinkpy_socket_has_socketcall_doc[] = ""
	"Returns C{True} if the socket calls - like connect, bind, sendto etc. - are\n"
	"implemented as subcalls of the socketcall(2) system call, C{False} otherwise.\n"
	"\n"
	"@note: Availability: Linux\n"
	"\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n";
static PyObject *
pinkpy_socket_has_socketcall(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	pink_bitness_t bit;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, "|I", &bit))
		return NULL;

	if (!check_bitness(bit))
		return NULL;

	if (pink_has_socketcall(bit)) {
		Py_INCREF(Py_True);
		return Py_True;
	}
	Py_INCREF(Py_False);
	return Py_False;
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char pinkpy_socket_name_doc[] = ""
	"Returns the name of the socket subcall.\n"
	"\n"
	"@note: Availability: Linux\n"
	"\n"
	"@param subcall: The socket subcall\n"
	"@rtype: str\n"
	"@return: The name of the socket subcall or C{None}";
static PyObject *
pinkpy_socket_name(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	long subno;
	const char *subname;

	if (!PyArg_ParseTuple(args, "l", &subno))
		return NULL;

	subname = pink_name_socket_subcall(subno);
	/* "s" returns None if subname is NULL */
	return Py_BuildValue("s", subname);
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char pinkpy_socket_decode_call_doc[] = ""
	"Returns the decoded socket call.\n"
	"\n"
	"@note: Availability: Linux\n"
	"@note: This function decodes the I{socketcall(2)} system call on some\n"
	"architectures. On others it's equivalent to pinktrace.syscall.get_no()\n"
	"\n"
	"@param pid: The process ID of the traced child\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: long\n"
	"@return: The decoded socket call";
static PyObject *
pinkpy_socket_decode_call(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	long subcall;
	pid_t pid;
	pink_bitness_t bit;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"|I", &pid, &bit))
		return NULL;

	if (!check_bitness(bit))
		return NULL;

	if (!pink_decode_socket_call(pid, bit, &subcall))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", subcall);
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static char pinkpy_socket_decode_fd_doc[] = ""
	"Returns the socket file descriptor.\n"
	"\n"
	"@note: Availability: Linux\n"
	"@note: This function decodes the I{socketcall(2)} system call on some\n"
	"architectures.\n"
	"\n"
	"@param index: The index of the argument\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than MAX_INDEX\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: long\n"
	"@return: The socket file descriptor";
static PyObject *
pinkpy_socket_decode_fd(PINK_GCC_ATTR((unused)) PyObject *self,
#if !defined(PINKTRACE_LINUX)
	PINK_GCC_ATTR((unused))
#endif
	PyObject *args)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	long fd;

	ind = 0;
	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"|II", &pid, &ind, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	if (!pink_decode_socket_fd(pid, bit, ind, &fd))
		return PyErr_SetFromErrno(PyExc_OSError);

	return Py_BuildValue("l", fd);
#else
	PyErr_SetString(PyExc_NotImplementedError, "Not implemented");
	return NULL;
#endif /* defined(PINKTRACE_LINUX) */
}

static PyObject *
Address_new(PyTypeObject *t, PINK_GCC_ATTR((unused)) PyObject *a, PINK_GCC_ATTR((unused)) PyObject *k)
{
	return t->tp_alloc(t, 0);
}

static void
Address_dealloc(PyObject *o)
{
	o->ob_type->tp_free(o);
}

static PyObject *
Address_family(PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
	Address *addr = (Address *)self;
#if PY_MAJOR_VERSION > 2
	return PyLong_FromLong(addr->addr.family);
#else
	return PyInt_FromLong(addr->addr.family);
#endif /* PY_MAJOR_VERSION > 2 */
}

static PyObject *
Address_length(PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
	Address *addr = (Address *)self;
#if PY_MAJOR_VERSION > 2
	return PyLong_FromLong(addr->addr.length);
#else
	return PyInt_FromLong(addr->addr.length);
#endif /* PY_MAJOR_VERSION > 2 */
}

static PyObject *
Address_abstract(PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_UNIX && IS_ABSTRACT(addr->addr)) {
		Py_INCREF(Py_True);
		return Py_True;
	}
	Py_INCREF(Py_False);
	return Py_False;
}

static PyObject *
Address_path(PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_UNIX)
#if PY_MAJOR_VERSION > 2
		return PyUnicode_FromString(addr->addr.u.sa_un.sun_path + (IS_ABSTRACT(addr->addr) ? 1 : 0));
#else
		return PyString_FromString(addr->addr.u.sa_un.sun_path + (IS_ABSTRACT(addr->addr) ? 1 : 0));
#endif /* PY_MAJOR_VERSION > 2 */
	PyErr_SetString(PyExc_TypeError, "Invalid family");
	return NULL;
}

static PyObject *
Address_ip(PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
	char *ip;
	PyObject *ipObj;
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_INET) {
		ip = PyMem_New(char, INET_ADDRSTRLEN);
		inet_ntop(AF_INET, &addr->addr.u.sa_in.sin_addr, ip, INET_ADDRSTRLEN);
#if PY_MAJOR_VERSION > 2
		ipObj = PyUnicode_FromString(ip);
#else
		ipObj = PyString_FromString(ip);
#endif /* PY_MAJOR_VERSION > 2 */
		PyMem_Free(ip);
		return ipObj;
	}
	PyErr_SetString(PyExc_TypeError, "Invalid family");
	return NULL;
}

#if PINKTRACE_HAVE_IPV6
static PyObject *
Address_ipv6(PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
	char *ip;
	PyObject *ipObj;
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_INET6) {
		ip = PyMem_New(char, INET6_ADDRSTRLEN);
		inet_ntop(AF_INET6, &addr->addr.u.sa6.sin6_addr, ip, INET6_ADDRSTRLEN);
		ipObj = PyUnicode_FromString(ip);
		PyMem_Free(ip);
		return ipObj;
	}
	PyErr_SetString(PyExc_TypeError, "Invalid family");
	return NULL;
}
#endif /* PINKTRACE_HAVE_IPV6 */

static PyObject *
Address_port(PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
	Address *addr = (Address *)self;

	switch (addr->addr.family) {
	case AF_INET:
#if PY_MAJOR_VERSION > 2
		return PyLong_FromLong(ntohs(addr->addr.u.sa_in.sin_port));
#else
		return PyInt_FromLong(ntohs(addr->addr.u.sa_in.sin_port));
#endif /* PY_MAJOR_VERSION > 2 */
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
#if PY_MAJOR_VERSION > 2
		return PyLong_FromLong(ntohs(addr->addr.u.sa6.sin6_port));
#else
		return PyInt_FromLong(ntohs(addr->addr.u.sa6.sin6_port));
#endif /* PY_MAJOR_VERSION > 2 */
#endif /* PINKTRACE_HAVE_IPV6 */
	default:
		PyErr_SetString(PyExc_TypeError, "Invalid family");
		return NULL;
	}
}

static PyObject *
Address_pid(
#if !PINKTRACE_HAVE_NETLINK
	PINK_GCC_ATTR((unused))
#endif /* !PINKTRACE_HAVE_NETLINK */
	PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
#if PINKTRACE_HAVE_NETLINK
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_NETLINK)
		return PyLong_FromPid(addr->addr.u.nl.nl_pid);
#endif /* PINKTRACE_HAVE_NETLINK */
	PyErr_SetString(PyExc_TypeError, "Invalid family");
	return NULL;
}

static PyObject *
Address_groups(
#if !PINKTRACE_HAVE_NETLINK
	PINK_GCC_ATTR((unused))
#endif /* !PINKTRACE_HAVE_NETLINK */
	PyObject *self, PINK_GCC_ATTR((unused)) void *x)
{
#if PINKTRACE_HAVE_NETLINK
	Address *addr = (Address *)self;

	if (addr->addr.family == AF_NETLINK)
		return PyLong_FromLong(addr->addr.u.nl.nl_groups);
#endif /* PINKTRACE_HAVE_NETLINK */
	PyErr_SetString(PyExc_TypeError, "Invalid family");
	return NULL;
}

static PyObject *
Address_repr(PyObject *self)
#if PY_MAJOR_VERSION > 2
{
	Address *addr = (Address *)self;

	switch (addr->addr.family) {
	case -1:
		return PyUnicode_FromString("<Address family=-1 addr=NULL>");
	case AF_UNIX:
		return PyUnicode_FromFormat("<Address family=AF_UNIX, addr=%p>",
			(void *)&addr->addr);
	case AF_INET:
		return PyUnicode_FromFormat("<Address family=AF_INET, addr=%p>",
			(void *)&addr->addr);
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
		return PyUnicode_FromFormat("<Address family=AF_INET6, addr=%p>",
			(void *)&addr->addr);
#endif /* PINKTRACE_HAVE_IPV6 */
#if PINKTRACE_HAVE_NETLINK
	case AF_NETLINK:
		return PyUnicode_FromFormat("<Address family=AF_NETLINK, addr=%p>",
			(void *)&addr->addr);
#endif /* PINKTRACE_HAVE_NETLINK */
	default:
		return PyUnicode_FromFormat("<Address family=%d, addr=NULL>",
			addr->addr.family);
	}
}
#else
{
	Address *addr = (Address *)self;

	switch (addr->addr.family) {
	case -1:
		return PyString_FromString("<Address family=-1 addr=NULL>");
	case AF_UNIX:
		return PyString_FromFormat("<Address family=AF_UNIX, addr=%p>",
			(void *)&addr->addr);
	case AF_INET:
		return PyString_FromFormat("<Address family=AF_INET, addr=%p>",
			(void *)&addr->addr);
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
		return PyString_FromFormat("<Address family=AF_INET6, addr=%p>",
			(void *)&addr->addr);
#endif /* PINKTRACE_HAVE_IPV6 */
#if PINKTRACE_HAVE_NETLINK
	case AF_NETLINK:
		return PyString_FromFormat("<Address family=AF_NETLINK, addr=%p>",
			(void *)&addr->addr);
#endif /* PINKTRACE_HAVE_NETLINK */
	default:
		return PyString_FromFormat("<Address family=%d, addr=NULL>",
			addr->addr.family);
	}
}
#endif /* PY_MAJOR_VERSION > 2 */

static struct PyGetSetDef Address_get_sets[] = {
	{"family", Address_family, 0, 0, 0},
	{"length", Address_length, 0, 0, 0},
	{"abstract", Address_abstract, 0, 0, 0},
	{"path", Address_path, 0, 0, 0},
	{"ip", Address_ip, 0, 0, 0},
#if PINKTRACE_HAVE_IPV6
	{"ipv6", Address_ipv6, 0, 0, 0},
#endif /* PINKTRACE_HAVE_IPV6 */
	{"port", Address_port, 0, 0, 0},
	{"pid", Address_pid, 0, 0, 0},
	{"groups", Address_groups, 0, 0, 0},
	{NULL, NULL, 0, 0, 0}
};

static char Address_doc[] = ""
	"This class represents a decoded socket address\n"
	"\n"
	"Methods:"
	"\n"
	"- B{family}:\n"
	"    - Returns the family of the Address (AF_UNIX, AF_INET, etc.)\n"
	"- B{length}:\n"
	"    - Returns the length of the Address\n"
	"- B{abstract}:\n"
	"    - Returns True if the Address represents an abstract UNIX socket, False otherwise\n"
	"- B{path}:\n"
	"    - Returns the path of the UNIX socket\n"
	"- B{ip}:\n"
	"    - Returns the ip address as a string\n"
#if PINKTRACE_HAVE_IPV6
	"- B{ipv6}:\n"
	"    - Returns the ipv6 address as a string\n"
#endif /* PINKTRACE_HAVE_IPV6 */
	"- B{port}:\n"
	"    - Returns the port\n"
	"- B{pid}:\n"
	"    - Returns the pid of the netlink socket Address\n"
	"- B{groups}:\n"
	"    - Returns the mcast groups mask of the netlink socket Address\n";
static PyTypeObject Address_type = {
#if PY_MAJOR_VERSION > 2
	PyVarObject_HEAD_INIT(NULL, 0)
#else
	PyObject_HEAD_INIT(NULL)
	0,						/* ob_size */
#endif /* PY_MAJOR_VERSION > 2 */
	"socket.Address",				/* tp_name */
	sizeof(Address),				/* tp_basicsize */
	0,						/* tp_itemsize */
	(destructor)Address_dealloc,			/* tp_dealloc */
	0,						/* tp_print */
	0,						/* tp_getattr */
	0,						/* tp_setattr */
	0,						/* tp_compare */
	(reprfunc)Address_repr,				/* tp_repr */
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
	Address_doc,					/* tp_doc */
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
	"@note: This function decodes the I{socketcall(2)} system call on some\n"
	"architectures.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than C{MAX_INDEX}\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: pinktrace.socket.Address\n"
	"@return: The decoded socket address";
static PyObject *
pinkpy_socket_decode_address(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	pink_bitness_t bit;
	PyObject *obj;
	Address *addr;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"I|I", &pid, &ind, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	obj = PyObject_CallObject((PyObject *)&Address_type, NULL);
	if (!obj)
		return NULL;

	addr = (Address *)obj;
	if (!pink_decode_socket_address(pid, bit, ind, NULL, &addr->addr))
		/* FIXME: Do we need to free obj here? */
		return PyErr_SetFromErrno(PyExc_OSError);

	return obj;
}

static char pinkpy_socket_decode_address_fd_doc[] = ""
	"Decodes the socket address at the given argument index;\n"
	"and the file descriptor at index 0.\n"
	"\n"
	"@note: This function decodes the I{socketcall(2)} system call on some\n"
	"architectures.\n"
	"\n"
	"@param pid: Process ID of the traced child\n"
	"@param index: The index of the argument\n"
	"@param bitness: The bitness of the traced child\n"
	"(Optional, defaults to C{pinktrace.bitness.DEFAULT_BITNESS})\n"
	"@raise IndexError: Raised if the index is not smaller than C{MAX_INDEX}\n"
	"@raise ValueError: Raised if the given bitness is either unsupported or invalid\n"
	"@raise OSError: Raised when the underlying I{ptrace(2)} call fails.\n"
	"@rtype: tuple\n"
	"@return: The decoded socket address and the file descriptor";
static PyObject *
pinkpy_socket_decode_address_fd(PINK_GCC_ATTR((unused)) PyObject *self, PyObject *args)
{
	pid_t pid;
	unsigned ind;
	long fd;
	pink_bitness_t bit;
	PyObject *obj;
	Address *addr;

	bit = PINKTRACE_BITNESS_DEFAULT;
	if (!PyArg_ParseTuple(args, PARSE_PID"I|I", &pid, &ind, &bit))
		return NULL;

	if (!check_bitness(bit) || !check_index(ind))
		return NULL;

	obj = PyObject_CallObject((PyObject *)&Address_type, NULL);
	if (!obj)
		return NULL;

	addr = (Address *)obj;
	if (!pink_decode_socket_address(pid, bit, ind, &fd, &addr->addr))
		/* FIXME: Do we need to free obj here? */
		return PyErr_SetFromErrno(PyExc_OSError);

	return PyTuple_Pack(2, obj, PyLong_FromLong(fd));
}

static void
socket_init(PyObject *mod)
{
	Py_INCREF(&Address_type);
	PyModule_AddObject(mod, "Address", (PyObject *)&Address_type);
}

static char socket_doc[] = "Pink's socket decoding functions";
static PyMethodDef socket_methods[] = {
	{"has_socketcall", pinkpy_socket_has_socketcall, METH_VARARGS, pinkpy_socket_has_socketcall_doc},
	{"name", pinkpy_socket_name, METH_VARARGS, pinkpy_socket_name_doc},
	{"decode_call", pinkpy_socket_decode_call, METH_VARARGS, pinkpy_socket_decode_call_doc},
	{"decode_fd", pinkpy_socket_decode_fd, METH_VARARGS, pinkpy_socket_decode_fd_doc},
	{"decode_address", pinkpy_socket_decode_address, METH_VARARGS, pinkpy_socket_decode_address_doc},
	{"decode_address_fd", pinkpy_socket_decode_address_fd, METH_VARARGS, pinkpy_socket_decode_address_fd_doc},
	{NULL, NULL, 0, NULL}
};

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef socket_module = {
	PyModuleDef_HEAD_INIT,
	"socket",
	socket_doc,
	-1,
	socket_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_socket(void)
{
	PyObject *mod;

	if (PyType_Ready(&Address_type) < 0)
		return NULL;

	mod = PyModule_Create(&socket_module);
	if (!mod)
		return NULL;

	socket_init(mod);

	return mod;
}
#else
PyMODINIT_FUNC
initsocket(void)
{
	PyObject *mod;

	if (PyType_Ready(&Address_type) < 0)
		return;

	mod = Py_InitModule3("socket", socket_methods, socket_doc);

	socket_init(mod);
}
#endif /* PY_MAJOR_VERSION > 2 */
