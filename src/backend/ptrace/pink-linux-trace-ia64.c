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

#include <asm/ptrace_offsets.h>
#include <asm/rse.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

#define ORIG_ACCUM	(PT_R15)

static bool
pink_trace_util_peek_ia64(pid_t pid, int narg, long *res)
{
	unsigned long *out0, cfm, sof, sol;
	long rbs_end;

	if (!pink_trace_util_peek(pid, PT_AR_BSP, &rbs_end))
		return false;
	if (!pink_trace_util_peek(pid, PT_CFM, (long *)&cfm))
		return false;

	sof = (cfm >> 0) & 0x7f;
	sol = (cfm >> 7) & 0x7f;
	out0 = ia64_rse_skip_regs((unsigned long *)rbs_end, -sof + sol);

	return pink_trace_util_moven(pid, (unsigned long)ia64_rse_skip_regs(out0, narg), (char *)res, sizeof(long));
}

pink_bitness_t
pink_trace_util_get_bitness(pink_unused pid_t pid)
{
	return PINK_BITNESS_64;
}

bool
pink_trace_util_get_syscall(pid_t pid, pink_unused pink_bitness_t bitness, long *res)
{
	return pink_trace_util_peek(pid, ORIG_ACCUM, res);
}

bool
pink_trace_util_set_syscall(pid_t pid, pink_unused pink_bitness_t bitness, long scno)
{
	return pink_trace_util_poke(pid, ORIG_ACCUM, scno);
}

bool
pink_trace_util_get_return(pid_t pid, long *res)
{
	long r8, r10;

	if (!pink_trace_util_peek(pid, PT_R8, &r8)
		|| !pink_trace_util_peek(pid, PT_R10, &r10))
		return false;

	*res = (r10 != 0) ? -r8 : r8;
	return true;
}

bool
pink_trace_util_set_return(pid_t pid, long ret)
{
	long r8, r10;

	r8 = (ret < 0) ? -ret : ret;
	r10 = (ret < 0) ? -1 : 0;

	return pink_trace_util_poke(pid, PT_R8, r8) &&
		pink_trace_util_poke(pid, PT_R10, r10);
}

bool
pink_trace_util_get_arg(pid_t pid, pink_unused pink_bitness_t bitness, unsigned ind, long *res)
{
	assert(ind < PINK_TRACE_MAX_INDEX);

	return pink_trace_util_peek_ia64(pid, ind, res);
}

bool
pink_trace_decode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, void *dest, size_t len)
{
	long addr;

	assert(ind < PINK_TRACE_MAX_INDEX);

	if (!pink_trace_util_get_arg(pid, bitness, ind, &addr))
		return false;

	return pink_trace_util_moven(pid, addr, dest, len);
}

bool
pink_trace_decode_string(pid_t pid, pink_bitness_t bitness, unsigned ind, char *dest, size_t len)
{
	long addr;

	assert(ind < PINK_TRACE_MAX_INDEX);

	if (!pink_trace_util_get_arg(pid, bitness, ind, &addr))
		return false;

	return pink_trace_util_movestr(pid, addr, dest, len);
}

char *
pink_trace_decode_string_persistent(pid_t pid, pink_bitness_t bitness, unsigned ind)
{
	long addr;

	assert(ind < PINK_TRACE_MAX_INDEX);

	if (!pink_trace_util_get_arg(pid, bitness, ind, &addr))
		return false;

	return pink_trace_util_movestr_persistent(pid, addr);
}

bool
pink_trace_encode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, const void *src, size_t len)
{
	long addr;

	assert(ind < PINK_TRACE_MAX_INDEX);

	if (!pink_trace_util_get_arg(pid, bitness, ind, &addr))
		return false;

	return pink_trace_util_putn(pid, addr, src, len);
}

bool
pink_trace_encode_simple_safe(pid_t pid, pink_bitness_t bitness, unsigned ind, const void *src, size_t len)
{
	long addr;

	assert(ind < PINK_TRACE_MAX_INDEX);

	if (!pink_trace_util_get_arg(pid, bitness, ind, &addr))
		return false;

	return pink_trace_util_putn_safe(pid, addr, src, len);
}

bool
pink_trace_decode_socket_call(pid_t pid, pink_bitness_t bitness, long *subcall_r)
{
	long addr;

	assert(subcall_r != NULL);

	/* No decoding needed */
	if (!pink_trace_util_get_syscall(pid, bitness, subcall_r))
		return false;

	return true;
}

bool
pink_trace_decode_socket_fd(pid_t pid, pink_bitness_t bitness, unsigned ind, long *fd)
{
	assert(fd != NULL);

	/* No decoding needed */
	return pink_trace_util_get_arg(pid, bitness, ind, fd);
}

bool
pink_trace_decode_socket_address(pid_t pid, pink_bitness_t bitness, unsigned ind,
	long *fd_r, pink_socket_address_t *addr_r)
{
	long addr, addrlen;

	/* No decoding needed */
	if (fd_r && !pink_trace_util_get_arg(pid, bitness, 0, fd_r))
		return false;
	if (!pink_trace_util_get_arg(pid, bitness, ind, &addr))
		return false;
	if (!pink_trace_util_get_arg(pid, bitness, ind + 1, &addrlen))
		return false;

	return pink_trace_internal_decode_socket_address(pid, addr, addrlen, addr_r);
}
