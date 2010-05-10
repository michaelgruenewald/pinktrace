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

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/pioctl.h>
#include <machine/reg.h>

#include <pinktrace/pink.h>

#define PROC_CURPROC_REGS "/proc/curproc/regs"

bool
pink_proc_util_open(pid_t pid, int *fd)
{
	char proc[32];

	if (pid < 0) {
		/* Open /proc/curproc/mem */
		*fd = open(PROC_CURPROC_REGS, O_RDWR);
	}
	else {
		snprintf(proc, 32, "/proc/%d/regs", pid);
		*fd = open(proc, O_RDWR);
	}

	if (*fd < 0)
		return false;

	return true;
}

bool
pink_proc_util_get_regs(int rfd, struct reg *regs)
{
	int ret;
	unsigned len;
	struct reg *r;

	if (lseek(rfd, 0L, SEEK_SET) < 0)
		return false;

	len = 0;
	r = regs;
	do {
		ret = read(rfd, r, sizeof(struct reg) - len);
		if (!ret)
			return false;
		else if (ret < 0) {
			if (errno == EINTR)
				continue;
			return false;
		}
		r += ret;
		len += ret;
	} while (len < sizeof(struct reg));

	return true;
}

bool
pink_proc_util_set_regs(int rfd, const struct reg *regs)
{
	int len, ret;
	const struct reg *r;

	if (lseek(rfd, 0L, SEEK_SET) < 0)
		return false;

	len = sizeof(struct reg);
	r = regs;
	do {
		ret = write(rfd, r, len);
		if (!ret)
			return false;
		else if (ret < 0) {
			if (errno == EINTR)
				continue;
			return false;
		}
		r += ret;
		len -= ret;
	} while (len > 0);

	return true;
}
