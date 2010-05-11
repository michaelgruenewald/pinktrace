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

#include <stdio.h> /* NULL */

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

#include "freebsd/pink-syscallent.h"
#include "freebsd/amd64/pink-syscallent32.h"

static int nsys = sizeof(sysnames) / sizeof(sysnames[0]);
static int nsys32 = sizeof(sysnames32) / sizeof(sysnames32[0]);

const char *
pink_name_syscall(long scno, pink_bitness_t bitness)
{
	int num;
	const char **names;

	switch (bitness) {
	case PINK_BITNESS_32:
		num = nsys32;
		names = sysnames32;
		break;
	case PINK_BITNESS_64:
		num = nsys;
		names = sysnames;
		break;
	default:
		return NULL;
	}

	if (scno < 0 || scno >= num)
		return NULL;
	return names[scno];
}

long
pink_name_lookup(const char *name, pink_bitness_t bitness)
{
	int n;
	long scno;
	const char **names;

	if (name == NULL || name[0] == '\0')
		return -1;

	switch (bitness) {
	case PINK_BITNESS_32:
		n = nsys32;
		names = sysnames32;
		break;
	case PINK_BITNESS_64:
		n = nsys;
		names = sysnames;
		break;
	default:
		return -1;
	}

	for (scno = 0; scno < n; scno++) {
		if (!strcmp(names[scno], name))
			return scno;
	}

	return -1;
}
