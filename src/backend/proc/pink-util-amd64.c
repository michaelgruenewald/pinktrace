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

#include <pinktrace/internal.h>

#include <sys/sysctl.h>

#include <pinktrace/pink.h>

struct bitness_types {
	const char *type;
	pink_bitness_t bitness;
} bitness_types[] = {
	{"FreeBSD ELF64", PINK_BITNESS_64},
	{"FreeBSD ELF32", PINK_BITNESS_32},
	{NULL, -1}
};

bool
pink_proc_util_bitness_get(pink_unused pid_t pid)
{
	char progt[32];
	size_t len = sizeof(progt);
	int mib[4];
	struct bitness_types *walk;

	mib[0] = CTL_KERN;
	mib[1] = KERN_PROC;
	mib[2] = KERN_PROC_SV_NAME;
	mib[3] = pid;

	if (sysctl(mib, 4, progt, &len, NULL, 0) < 0)
		return PINK_BITNESS_UNKNOWN;

	for (walk = bitness_types; walk->type; walk++) {
		if (!strcmp(walk->type, progt))
			return walk->bitness;
	}

	return PINK_BITNESS_UNKNOWN;
}
