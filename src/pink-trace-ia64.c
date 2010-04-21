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

#include <asm/ptrace_offsets.h>
#include <asm/rse.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

#define ORIG_ACCUM	(PT_R15)

pink_bitness_t
pink_bitness_get(pink_unused pid_t pid)
{
	return PINK_BITNESS_64;
}

bool
pink_util_get_syscall(pid_t pid, long *res)
{
	return pink_util_upeek(pid, ORIG_ACCUM, res);
}

bool
pink_util_set_syscall(pid_t pid, long scno)
{
	return (0 == ptrace(PTRACE_POKEUSER, pid, ORIG_ACCUM, scno));
}

bool
pink_util_get_return(pid_t pid, long *res)
{
	long r8, r10;

	if (!ptrace_util_upeek(pid, PT_R8, &r8)
		|| !ptrace_util_upeek(pid, PT_R10, &r10))
		return false;

	*res = (r10 != 0) ? -r8 : r8;
	return true;
}

bool
pink_util_set_return(pid_t pid, long ret)
{
	long r8, r10;

	r8 = (val < 0) ? -val : val;
	r10 = (val < 0) ? -1 : 0;

	return (0 == ptrace(PTRACE_POKEUSER, pid, PT_R8, r8)) &&
		(0 == ptrace(PTRACE_POKEUSER, pid, PT_R10, r10));
}
