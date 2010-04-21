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

#include <errno.h>
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

	*res = val;
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

		errno = 0;
		u.val = ptrace(PTRACE_PEEKDATA, pid, (char *)addr, NULL);
		if (errno) {
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
		errno = 0;
		u.val = ptrace(PTRACE_PEEKDATA, pid, (char *)addr, NULL);
		if (errno) {
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

		errno = 0;
		u.val = ptrace(PTRACE_PEEKDATA, pid, (char *)addr, NULL);
		if (errno) {
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
		errno = 0;
		u.val = ptrace(PTRACE_PEEKDATA, pid, (char *)addr, NULL);
		if (errno) {
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
