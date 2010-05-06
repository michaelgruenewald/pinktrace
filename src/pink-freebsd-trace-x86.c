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

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <sys/types.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

bool
pink_util_get_syscall(pid_t pid, long *res)
{
	unsigned parm_offset;
	struct reg r;

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	/*
	 * FreeBSD has two special kinds of system call redirections --
	 * SYS_syscall, and SYS___syscall.  The former is the old syscall()
	 * routine, basicly; the latter is for quad-aligned arguments.
	 */
	parm_offset = r.r_esp + sizeof(int);
	*res = r.r_eax;
	switch (*res) {
	case SYS_syscall:
		*res = ptrace(PT_READ_D, pid, (caddr_t)parm_offset, 0);
		if (*res < 0)
			return false;
		return true;
	case SYS___syscall:
		*res = ptrace(PT_READ_D, pid, (caddr_t)parm_offset, 0);
		if (*res < 0)
			return false;
		return true;
	default:
		return true;
	}
}

bool
pink_util_set_syscall(pid_t pid, long scno)
{
	struct reg r;

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	r.r_eax = scno;

	if (0 > ptrace(PT_SETREGS, pid, (caddr_t)&r, 0))
		return false;

	return true;
}

bool
pink_util_get_return(pid_t pid, long *res)
{
	bool errorp;
	struct reg r;

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	if (res) {
		errorp = !!(r.r_eflags & PSL_C);
		*res = errorp ? -r.r_eax : r.r_eax;
	}

	return true;
}

bool
pink_util_set_return(pid_t pid, long ret)
{
	struct reg r;

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	if (ret < 0) {
		r.r_eax = -ret;
		r.r_eflags |= PSL_C;
	}
	else
		r.r_eax = ret;

	if (0 > ptrace(PT_SETREGS, pid, (caddr_t)&r, 0))
		return false;

	return true;
}

bool
pink_util_get_arg(pid_t pid, pink_unused pink_bitness_t bitness, unsigned ind, long *res)
{
	unsigned parm_offset;
	long scno, arg;
	struct reg r;

	assert(ind < PINK_MAX_INDEX);

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
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
	errno = 0;
	arg = ptrace(PT_READ_D, pid, (caddr_t)parm_offset, 0);
	if (arg < 0 && errno != 0)
		return false;

	*res = arg;
	return true;
}
