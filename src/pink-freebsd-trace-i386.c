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
#include <stdbool.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <sys/types.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

pink_bitness_t
pink_bitness_get(pink_unused pid_t pid)
{
	return PINK_BITNESS_32;
}

bool
pink_util_get_syscall(pid_t pid, pink_unused pink_bitness_t bitness, long *res)
{
	unsigned parm_offset;
	struct reg r;

	if (!pink_util_get_regs(pid, &r))
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
		if (!pink_util_peekdata(pid, parm_offset, res))
			return false;
		return true;
	default:
		return true;
	}
}

bool
pink_util_set_syscall(pid_t pid, pink_unused pink_bitness_t bitness, long scno)
{
	struct reg r;

	if (!pink_util_get_regs(pid, &r))
		return false;

	r.r_eax = scno;

	if (!pink_util_set_regs(pid, &r))
		return false;

	return true;
}

bool
pink_util_get_return(pid_t pid, long *res)
{
	bool errorp;
	struct reg r;

	if (!pink_util_get_regs(pid, &r))
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

	if (!pink_util_get_regs(pid, &r))
		return false;

	if (ret < 0) {
		r.r_eax = -ret;
		r.r_eflags |= PSL_C;
	}
	else
		r.r_eax = ret;

	if (!pink_util_set_regs(pid, &r))
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

	if (!pink_util_get_regs(pid, &r))
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
	if (!pink_util_peekdata(pid, parm_offset, &arg))
		return false;

	*res = arg;
	return true;
}

bool
pink_decode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, void *dest, size_t len)
{
	long addr;

	assert(ind < PINK_MAX_INDEX);

	return pink_util_get_arg(pid, bitness, ind, &addr) && pink_util_moven(pid, addr, dest, len);
}

bool
pink_decode_string(pid_t pid, pink_bitness_t bitness, unsigned ind, char *dest, size_t len)
{
	long addr;

	assert(ind < PINK_MAX_INDEX);

	return pink_util_get_arg(pid, bitness, ind, &addr) && pink_util_movestr(pid, addr, dest, len);
}

char *
pink_decode_string_persistent(pid_t pid, pink_bitness_t bitness, unsigned ind)
{
	long addr;

	assert(ind < PINK_MAX_INDEX);

	if (!pink_util_get_arg(pid, bitness, ind, &addr))
		return NULL;

	return pink_util_movestr_persistent(pid, addr);
}

bool
pink_decode_socket_address(pid_t pid, pink_bitness_t bitness, unsigned ind, long *fd_r, pink_socket_address_t *addr_r)
{
	long addr;
	long addrlen;

	if (fd_r && !pink_util_get_arg(pid, bitness, 0, fd_r))
		return false;

	if (!pink_util_get_arg(pid, bitness, ind, &addr) || !pink_util_get_arg(pid, bitness, ind + 1, &addrlen))
		return false;

	return pink_internal_decode_socket_address(pid, addr, addrlen, addr_r);
}

bool
pink_encode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, const void *src, size_t len)
{
	long addr;

	assert(ind < PINK_MAX_INDEX);

	return pink_util_get_arg(pid, bitness, ind, &addr) && pink_util_putn(pid, addr, src, len);
}
