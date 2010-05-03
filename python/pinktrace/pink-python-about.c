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
PyInit_about(void);
#else
initabout(void);
#endif /* PY_MAJOR_VERSION > 2 */

static char about_doc[] = "Pink's version and build constants";
static PyMethodDef about_methods[] = {
	{NULL, NULL, 0, NULL}
};

static void
about_init(PyObject *mod)
{
	PyModule_AddIntConstant(mod, "VERSION_MAJOR", PINKTRACE_VERSION_MAJOR);
	PyModule_AddIntConstant(mod, "VERSION_MINOR", PINKTRACE_VERSION_MINOR);
	PyModule_AddIntConstant(mod, "VERSION_MICRO", PINKTRACE_VERSION_MICRO);
	PyModule_AddIntConstant(mod, "VERSION", PINKTRACE_VERSION);
	PyModule_AddIntConstant(mod, "__version__", PINKTRACE_VERSION);
	PyModule_AddStringConstant(mod, "VERSION_SUFFIX", PINKTRACE_VERSION_SUFFIX);
	PyModule_AddStringConstant(mod, "GIT_HEAD", PINKTRACE_GIT_HEAD);
	PyModule_AddIntConstant(mod, "HAVE_IPV6", PINKTRACE_HAVE_IPV6);
}

#if PY_MAJOR_VERSION > 2
static struct PyModuleDef about_module = {
	PyModuleDef_HEAD_INIT,
	"about",
	about_doc,
	-1,
	about_methods,
	NULL,
	NULL,
	NULL,
	NULL
};

PyMODINIT_FUNC
PyInit_about(void)
{
	PyObject *mod;

	mod = PyModule_Create(&about_module);
	if (!mod)
		return NULL;

	about_init(mod);

	return mod;
}
#else
PyMODINIT_FUNC
initabout(void)
{
	PyObject *mod = Py_InitModule3("about", about_methods, about_doc);
	if (!mod)
		return;

	about_init(mod);
}
#endif /* PY_MAJOR_VERSION > 2 */
