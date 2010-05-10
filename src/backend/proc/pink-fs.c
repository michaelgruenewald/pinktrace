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

#include <pinktrace/pink.h>

#define PROC_CURPROC_MEM "/proc/curproc/mem"

bool
pink_proc_open(pid_t pid, int *fd)
{
	char proc[32];

	if (pid < 0) {
		/* Open /proc/curproc/mem */
		*fd = open(PROC_CURPROC_MEM, O_RDWR);
	}
	else {
		snprintf(proc, 32, "/proc/%d/mem", pid);
		*fd = open(proc, O_RDWR);
	}

	if (*fd < 0)
		return false;

	return true;
}

bool
pink_proc_get_flags(int fd, int *flags)
{
	return !(ioctl(fd, PIOCGFL, flags) < 0);
}

bool
pink_proc_set_flags(int fd, int flags)
{
	return !(ioctl(fd, PIOCSFL, flags) < 0);
}

bool
pink_proc_set_event_flags(int fd, int flags)
{
	return !(ioctl(fd, PIOCBIS, flags) < 0);
}

bool
pink_proc_clear_event_flags(int fd)
{
	return !(ioctl(fd, PIOCBIC, ~0) < 0);
}

bool
pink_proc_cont(int fd)
{
	return !(ioctl(fd, PIOCCONT, 0) < 0);
}

bool
pink_proc_wait(int fd, struct procfs_status *status)
{
	return !(ioctl(fd, PIOCWAIT, status) < 0);
}

bool
pink_proc_status(int fd, struct procfs_status *status)
{
	return !(ioctl(fd, PIOCSTATUS, status) < 0);
}

bool
pink_proc_read(int fd, off_t off, void *dest, size_t len)
{
	return pread(fd, dest, len, off) == (ssize_t)len;
}

bool
pink_proc_write(int fd, off_t off, const void *src, size_t len)
{
	return pwrite(fd, src, len, off) == (ssize_t)len;
}
