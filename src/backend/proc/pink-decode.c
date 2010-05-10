/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon truss which is:
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

#include <pinktrace/internal.h>

#include <stdbool.h>
#include <sys/types.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <pinktrace/pink.h>

bool
pink_proc_decode_simple(int fd, int rfd, pink_bitness_t bitness, unsigned ind, void *dest, size_t len)
{
	unsigned long off;

	if (!pink_proc_util_get_arg(fd, rfd, bitness, ind, &off))
		return false;

	return pink_proc_read(fd, (off_t)off, dest, len);
}

bool
pink_proc_decode_string(int fd, int rfd, pink_bitness_t bitness, unsigned ind, char *dest, size_t len)
{
	return pink_proc_decode_simple(fd, rfd, bitness, ind, dest, len);
}

char *
pink_proc_decode_string_persistent(int fd, int rfd, pink_bitness_t bitness, unsigned ind)
{
	int dfd;
	int c, len, size;
	int save_errno;
	unsigned long off;
	char *buf;
	FILE *fp;

	if (!pink_proc_util_get_arg(fd, rfd, bitness, ind, &off))
		return false;

	dfd = -1;
	buf = NULL;
	fp = NULL;

	if ((dfd = dup(fd)) < 0)
		return NULL;

	if ((fp = fdopen(dfd, "r")) == NULL)
		goto end;

	if (fseek(fp, off, SEEK_SET) != 0)
		goto end;

	size = 64;
	buf = malloc(sizeof(char) * size);
	if (!buf)
		goto end;
	len = 0;
	buf[0] = '\0';

	while ((c = fgetc(fp)) != EOF) {
		buf[len++] = c;
		if (c == 0) {
			buf[len] = '\0';
			break;
		}
		if (len == size) {
			char *tmp;
			tmp = realloc(buf, size + 64);
			if (!tmp) {
				/* XXX: Maybe we should die here as well? */
				buf[len] = '\0';
				break;
			}
			size += 64;
			buf = tmp;
		}
	}

end:
	if (dfd > 0) {
		save_errno = errno;
		close(dfd);
		errno = save_errno;
	}
	if (fp) {
		save_errno = errno;
		fclose(fp);
		errno = save_errno;
	}
	return buf;
}
