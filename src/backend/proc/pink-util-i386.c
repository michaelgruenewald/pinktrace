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

#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <sys/syscall.h>

#include <pinktrace/pink.h>

pink_bitness_t
pink_proc_util_get_bitness(pink_unused pid_t pid)
{
	return PINK_BITNESS_32;
}

bool
pink_proc_util_get_syscall(int fd, int rfd, pink_unused pink_bitness_t bitness, long *res)
{
	unsigned parm_offset;
	struct reg r;

	if (!pink_proc_util_get_regs(rfd, &r))
		return false;

	/*
	 * FreeBSD has two special kinds of system call redirections --
	 * SYS_syscall, and SYS___syscall.  The former is the old syscall()
	 * routine, basicly; the latter is for quad-aligned arguments.
	 */
	*res = r.r_eax;
	switch (*res) {
	case SYS_syscall:
	case SYS___syscall:
		parm_offset = r.r_esp + sizeof(int);
		if (lseek(fd, parm_offset, SEEK_SET) < 0 || read(fd, res, sizeof(int)) != sizeof(int))
			return false;
		return true;
	default:
		return true;
	}
}

bool
pink_proc_util_set_syscall(pink_unused int fd, int rfd, pink_unused pink_bitness_t bitness, long scno)
{
	struct reg r;

	if (!pink_proc_util_get_regs(rfd, &r))
		return false;

	r.r_eax = scno;

	/* FIXME: Do we want to handle redirection here as well? */
	if (!pink_proc_util_set_regs(rfd, &r))
		return false;

	return true;
}

bool
pink_proc_util_get_return(int rfd, long *res)
{
	bool errorp;
	struct reg r;

	if (!pink_proc_util_get_regs(rfd, &r))
		return false;

	if (res) {
		errorp = !!(r.r_eflags & PSL_C);
		*res = errorp ? -r.r_eax : r.r_eax;
	}

	return true;
}

bool
pink_proc_util_set_return(int rfd, long ret)
{
	struct reg r;

	if (!pink_proc_util_get_regs(rfd, &r))
		return false;

	if (ret < 0) {
		r.r_eax = -ret;
		r.r_eflags |= PSL_C;
	}
	else
		r.r_eax = ret;

	if (!pink_proc_util_set_regs(rfd, &r))
		return false;

	return true;
}

bool
pink_proc_util_get_arg(int fd, int rfd, pink_unused pink_bitness_t bitness, unsigned ind, long *res)
{
	unsigned parm_offset;
	long scno;
	struct reg r;

	if (!pink_proc_util_get_regs(rfd, &r))
		return false;

	/*
	 * FreeBSD has two special kinds of system call redirctions --
	 * SYS_syscall, and SYS___syscall.  The former is the old syscall()
	 * routine, basicly; the latter is for quad-aligned arguments.
	 */
	parm_offset = r.r_esp + sizeof(int);
	scno = r.r_eax;
	switch (scno) {
	case SYS_syscall:
		parm_offset += sizeof(int);
		break;
	case SYS___syscall:
		parm_offset += sizeof(quad_t);
		break;
	default:
		break;
	}

	parm_offset += ind * sizeof(int);
	if (lseek(fd, parm_offset, SEEK_SET) < 0 || read(fd, res, sizeof(int)) != sizeof(int))
		return false;

	return true;
}
