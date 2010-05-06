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

#if defined(PINKTRACE_FREEBSD)
#include <sys/syscall.h>
#elif defined(PINKTRACE_LINUX)
#include <asm/unistd.h>
#else
#error unsupported operating system
#endif

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

static const struct {
    int no;
    const char *name;
} sysnames[] = {
#if defined(PINKTRACE_FREEBSD)
#include "pink-freebsd-syscallent.h"
#elif defined(PINKTRACE_LINUX)
#include "pink-linux-syscallent.h"
#else
#error unsupported operating system
#endif
    {-1,    NULL}
};

const char *
pink_name_syscall_nobitness(long scno)
{
	for (unsigned int i = 0; sysnames[i].name != NULL; i++)
		if (scno == sysnames[i].no)
			return sysnames[i].name;
	return NULL;
}
