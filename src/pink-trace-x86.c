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

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

#define ORIG_ACCUM	(4 * ORIG_EAX)
#define ACCUM		(4 * EAX)
static const long syscall_args[1][MAX_ARGS] = {
	{4 * EBX, 4 * ECX, 4 * EDX, 4 * ESI, 4 * EDI, 4 * EBP}
};

pink_bitness_t
pink_bitness_get(pink_unused pid_t pid)
{
	return PINK_BITNESS_32;
}

bool
pink_util_get_syscall(pid_t pid, long *res)
{
	return pink_util_peek(pid, ORIG_ACCUM, res);
}

bool
pink_util_set_syscall(pid_t pid, long scno)
{
	return (0 == ptrace(PTRACE_POKEUSER, pid, ORIG_ACCUM, scno));
}

bool
pink_util_get_return(pid_t pid, long *res)
{
	return pink_util_peek(pid, ACCUM, res);
}

bool
pink_util_set_return(pid_t pid, long ret)
{
	return (0 == ptrace(PTRACE_POKEUSER, pid, ACCUM, ret));
}

bool
pink_util_get_arg(pid_t pid, pink_bitness_t bitness, int arg, long *res)
{
	assert(bitness == PINK_BITNESS_32);
	assert(arg >= 0 && arg < MAX_ARGS);

	return pink_util_peek(pid, syscall_args[bitness][arg], res);
}

bool
pink_decode_string(pid_t pid, pink_bitness_t bitness, int arg, char *dest, size_t len)
{
	long addr;

	assert(bitness == PINK_BITNESS_32);
	assert(arg >= 0 && arg < MAX_ARGS);

	if (!pink_util_peek(pid, syscall_args[bitness][arg], &addr))
		return false;

	return pink_util_movestr(pid, addr, dest, len);
}

char *
pink_decode_string_persistent(pid_t pid, pink_bitness_t bitness, int arg)
{
	long addr;

	assert(bitness == PINK_BITNESS_32);
	assert(arg >= 0 && arg < MAX_ARGS);

	if (!pink_util_peek(pid, syscall_args[bitness][arg], &addr))
		return false;

	return pink_util_movestr_persistent(pid, addr);
}
