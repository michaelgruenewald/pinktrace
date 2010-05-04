/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon strace which is:
 *   Copyright (c) 1991, 1992 Paul Kranenburg <pk@cs.few.eur.nl>
 *   Copyright (c) 1993 Branko Lankester <branko@hacktic.nl>
 *   Copyright (c) 1993, 1994, 1995, 1996 Rick Sladkey <jrs@world.std.com>
 *   Copyright (c) 1996-1999 Wichert Akkerman <wichert@cistron.nl>
 *   Copyright (c) 1999 IBM Deutschland Entwicklung GmbH, IBM Corporation
 *                       Linux for s390 port by D.J. Barrow
 *                       <barrow_dj@mail.yahoo.com,djbarrow@de.ibm.com>
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

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

bool
pink_util_peek(pid_t pid, long off, long *res)
{
	long val;

	errno = 0;
	val = ptrace(PTRACE_PEEKUSER, pid, off, NULL);
	if (val == -1 && errno != 0)
		return false;

	if (res)
		*res = val;

	return true;
}

bool
pink_util_peekdata(pid_t pid, long off, long *res)
{
	long val;

	errno = 0;
	val = ptrace(PTRACE_PEEKDATA, pid, off, NULL);
	if (val == -1 && errno != 0)
		return false;

	if (res)
		*res = val;

	return true;
}

bool
pink_util_poke(pid_t pid, long off, long val)
{
	return (0 == ptrace(PTRACE_POKEUSER, pid, off, val));
}

bool
pink_util_pokedata(pid_t pid, long off, long val)
{
	return (0 == ptrace(PTRACE_POKEDATA, pid, off, val));
}

bool
pink_util_putn(pid_t pid, long addr, const char *src, size_t len)
{
	int n, m;
	union {
		long val;
		char x[sizeof(long)];
	} u;

	n = 0;
	m = len / sizeof(long);

	while (n < m) {
		memcpy(u.x, src, sizeof(long));
		if (!pink_util_pokedata(pid, addr + n * ADDR_MUL, u.val))
			return false;
		++n;
		src += sizeof(long);
	}

	m = len % sizeof(long);
	if (m) {
		memcpy(u.x, src, m);
		if (!pink_util_pokedata(pid, addr + n * ADDR_MUL, u.val))
			return false;
	}

	return true;
}

bool
pink_util_putn_safe(pid_t pid, long addr, const char *src, size_t len)
{
	int n, m;
	union {
		long val;
		char x[sizeof(long)];
	} u;

	n = 0;
	m = len / sizeof(long);

	while (n < m) {
		memcpy(u.x, src, sizeof(long));
		if (!pink_util_peekdata(pid, addr + n * ADDR_MUL, NULL))
			return false;
		if (!pink_util_pokedata(pid, addr + n * ADDR_MUL, u.val))
			return false;
		++n;
		src += sizeof(long);
	}

	m = len % sizeof(long);
	if (m) {
		memcpy(u.x, src, m);
		if (!pink_util_peekdata(pid, addr + n * ADDR_MUL, NULL))
			return false;
		if (!pink_util_pokedata(pid, addr + n * ADDR_MUL, u.val))
			return false;
	}

	return true;
}

#define MIN(a,b)	(((a) < (b)) ? (a) : (b))
bool
pink_util_moven(pid_t pid, long addr, char *dest, size_t len)
{
	int n, m;
	int started = 0;
	union {
		long val;
		char x[sizeof(long)];
	} u;

	if (addr & (sizeof(long) -1)) {
		/* addr not a multiple of sizeof(long) */
		n = addr - (addr & -sizeof(long)); /* residue */
		addr &= -sizeof(long); /* residue */

		if (!pink_util_peekdata(pid, addr, &u.val)) {
			if (started && (errno == EPERM || errno == EIO)) {
				/* Ran into end of memory */
				return true;
			}
			/* But if not started, we had a bogus address */
			return false;
		}
		started = 1;
		memcpy(dest, &u.x[n], m = MIN(sizeof(long) - n, len));
		addr += sizeof(long), dest += m, len -= m;
	}
	while (len > 0) {
		if (!pink_util_peekdata(pid, addr, &u.val)) {
			if (started && (errno == EPERM || errno == EIO)) {
				/* Ran into end of memory */
				return true;
			}
			/* But if not started, we had a bogus address */
			return false;
		}
		started = 1;
		memcpy(dest, u.x, m = MIN(sizeof(long), len));
		addr += sizeof(long), dest += m, len -= m;
	}
	return true;
}

bool
pink_util_movestr(pid_t pid, long addr, char *dest, size_t len)
{
	int n, m;
	int started = 0;
	union {
		long val;
		char x[sizeof(long)];
	} u;

	if (addr & (sizeof(long) -1)) {
		/* addr not a multiple of sizeof(long) */
		n = addr - (addr & -sizeof(long)); /* residue */
		addr &= -sizeof(long); /* residue */

		if (!pink_util_peekdata(pid, addr, &u.val)) {
			if (started && (errno == EPERM || errno == EIO)) {
				/* Ran into end of memory */
				return true;
			}
			/* But if not started, we had a bogus address */
			return false;
		}
		started = 1;
		memcpy(dest, &u.x[n], m = MIN(sizeof(long) - n, len));
		while (n & (sizeof(long) - 1))
			if (u.x[n++] == '\0')
				return true;
		addr += sizeof(long), dest += m, len -= m;
	}
	while (len > 0) {
		if (!pink_util_peekdata(pid, addr, &u.val)) {
			if (started && (errno == EPERM || errno == EIO)) {
				/* Ran into end of memory */
				return true;
			}
			/* But if not started, we had a bogus address */
			return false;
		}
		started = 1;
		memcpy(dest, u.x, m = MIN(sizeof(long), len));
		for (unsigned int i = 0; i < sizeof(long); i++)
			if (u.x[i] == '\0')
				return true;
		addr += sizeof(long), dest += m, len -= m;
	}
	return true;
}

char *
pink_util_movestr_persistent(pid_t pid, long addr)
{
	int n, m, sum;
	bool started;
	union {
		long val;
		char x[sizeof(long)];
	} u;
	char *res, *res_ptr;

#define XFREE(x)			\
	do {				\
		if ((x)) {		\
			free((x));	\
		}			\
	} while (0)

	started = false;
	res = res_ptr = NULL;
	sum = 0;

	if (addr & (sizeof(long) -1)) {
		/* addr not a multiple of sizeof(long) */
		n = addr - (addr & -sizeof(long)); /* residue */
		addr &= -sizeof(long); /* residue */

		if (!pink_util_peekdata(pid, addr, &u.val)) {
			if (started && (errno == EPERM || errno == EIO)) {
				/* Ran into end of memory */
				return res;
			}
			/* But if not started, we had a bogus address */
			XFREE(res);
			return NULL;
		}
		m = sizeof(long) - n;
		sum += m;
		if ((res = realloc(res, sum)) == NULL)
			return NULL;
		res_ptr = started ? res + (sum - m) : res;
		started = true;
		memcpy(res_ptr, &u.x[n], m);
		while (n & (sizeof(long) - 1))
			if (u.x[n++] == '\0')
				return res;
		addr += sizeof(long), res_ptr += m;
	}
	for (;;) {
		if (!pink_util_peekdata(pid, addr, &u.val)) {
			if (started && (errno == EPERM || errno == EIO)) {
				/* Ran into end of memory */
				return res;
			}
			/* But if not started, we had a bogus address */
			XFREE(res);
			return NULL;
		}
		m = sizeof(long);
		sum += m;
		if ((res = realloc(res, sum)) == NULL)
			return NULL;
		res_ptr = started ? res + (sum - m) : res;
		started = true;
		memcpy(res_ptr, u.x, m);
		for (unsigned int i = 0; i < sizeof(long); i++)
			if (u.x[i] == '\0')
				return res;
		addr += sizeof(long), res_ptr += m;
	}
	/* never reached */
	assert(false);
#undef XFREE
}
