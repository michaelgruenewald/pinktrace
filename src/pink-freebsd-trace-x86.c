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

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/types.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

bool
pink_util_get_syscall(pid_t pid, long *res)
{
	struct reg r;

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	if (res)
		*res = r.r_eax;

	return true;
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
	struct reg r;

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	if (res)
		*res = r.r_eax;

	return true;
}

bool
pink_util_set_return(pid_t pid, long ret)
{
	struct reg r;

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	r.r_eax = ret;

	if (0 > ptrace(PT_SETREGS, pid, (caddr_t)&r, 0))
		return false;

	return true;
}

bool
pink_util_get_arg(pid_t pid, pink_unused pink_bitness_t bitness, unsigned ind, long *res)
{
	struct reg r;

	assert(ind < PINK_MAX_INDEX);

	if (0 > ptrace(PT_GETREGS, pid, (caddr_t)&r, 0))
		return false;

	if (res) {
		switch (ind) {
		case 0:
			*res = r.r_ebx;
			break;
		case 1:
			*res = r.r_ecx;
			break;
		case 2:
			*res = r.r_edx;
			break;
		case 3:
			*res = r.r_esi;
			break;
		case 4:
			*res = r.r_edi;
			break;
		case 5:
			*res = r.r_ebp;
			break;
		default:
			abort();
		}
	}

	return true;
}
