/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon truss' i386-fbsd.c which is:
 *   Copyright 1997 Sean Eric Fagan
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

#include <stdlib.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

bool
pink_util_moven(pid_t pid, long addr, char *dest, size_t len)
{
	struct ptrace_io_desc ioreq;

	ioreq.piod_op = PIOD_READ_D;
	ioreq.piod_offs = (char *)addr;
	ioreq.piod_addr = dest;
	ioreq.piod_len = len;

	return !(ptrace(PT_IO, pid, (caddr_t)&ioreq, 0) < 0);
}

bool
pink_util_movestr(pid_t pid, long addr, char *dest, size_t len)
{
	return pink_util_moven(pid, addr, dest, len);
}

#define MAXSIZE 4096
#define BLOCKSIZE 1024

char *
pink_util_movestr_persistent(pid_t pid, long addr)
{
	int diff;
	size_t totalsize, size;
	char *buf;

	diff = 0;
	totalsize = size = BLOCKSIZE;
	buf = malloc(sizeof(char) * totalsize);
	if (!buf)
		return NULL;
	for (;;) {
		diff = totalsize - size;
		if (!pink_util_moven(pid, addr + diff, buf + diff, size)) {
			free(buf);
			return NULL;
		}
		for (unsigned int i = 0; i < size; i++) {
			if (buf[diff + i] == '\0')
				return buf;
		}
		if (totalsize < MAXSIZE - BLOCKSIZE) {
			totalsize += BLOCKSIZE;
			buf = realloc(buf, totalsize);
			if (!buf)
				return NULL;
			size = BLOCKSIZE;
		}
		else {
			buf[totalsize] = '\0';
			return buf;
		}
	}
}

bool
pink_util_putn(pid_t pid, long addr, const char *src, size_t len)
{
	struct ptrace_io_desc ioreq;

	ioreq.piod_op = PIOD_WRITE_D;
	ioreq.piod_offs = (char *)addr;
	ioreq.piod_addr = src;
	ioreq.piod_len = len;

	return !(ptrace(PT_IO, pid, (caddr_t)&ioreq, 0) < 0);
}
