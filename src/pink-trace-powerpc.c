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
#include <pinktrace/pink.h>

#ifndef PT_ORIG_R3
#define PT_ORIG_R3 34
#endif /* !PT_ORIG_R3 */

#define ORIG_ACCUM	(sizeof(unsigned long) * PT_R0)
#define ACCUM		(sizeof(unsigned long) * PT_R3)
#define ACCUM_FLAGS	(sizeof(unsigned long) * PT_CCR)
#define SO_MASK		0x10000000

#define ARG_OFFSET(i)	(((i) == 0)				\
		? (sizeof(unsigned long) * PT_ORIG_R3)		\
		: (sizeof(unsigned long) * ((i) + PT_R3)))

pink_bitness_t
pink_bitness_get(pink_unused pid_t pid)
{
#if defined(POWERPC)
	return PINK_BITNESS_32;
#elif defined(POWERPC64)
	return PINK_BITNESS_64;
#else
#error unsupported architecture
#endif
}

bool
pink_util_get_syscall(pid_t pid, long *res)
{
	return pink_util_peek(pid, ORIG_ACCUM, res);
}

bool
pink_util_set_syscall(pid_t pid, long scno)
{
	return pink_util_poke(pid, ORIG_ACCUM, scno);
}

bool
pink_util_get_return(pid_t pid, long *res)
{
	long flags;

	if (!pink_util_peek(pid, ACCUM, res) ||
			pink_util_peek(pid, ACCUM_FLAGS, &flags))
		return false;

	if (flags & SO_MASK)
		*res = -(*res);
	return true;
}

bool
pink_util_set_return(pid_t pid, long ret)
{
	long flags;

	if (!pink_util_peek(pid, ACCUM_FLAGS, &flags))
		return false;

	if (val < 0) {
		flags |= SO_MASK;
		val = -val;
	}
	else
		flags &= ~SO_MASK;

	return pink_util_poke(pid, ACCUM, ret) &&
		pink_util_poke(pid, ACCUM_FLAGS, ret);
}

bool
pink_util_get_arg(pid_t pid, pink_unused pink_bitness_t bitness, int arg, long *res)
{
	assert(arg >= 0 && arg < MAX_ARGS);

	return pink_util_peek(pid, ARG_OFFSET(arg), res);
}

bool
pink_decode_simple(pid_t pid, pink_bitness_t bitness, int arg, void *dest, size_t len)
{
	long addr;

	assert(arg >= 0 && arg < MAX_ARGS);

	if (!pink_util_get_arg(pid, bitness, arg, &addr))
		return false;

	return pink_util_moven(pid, addr, dest, len);
}

bool
pink_decode_string(pid_t pid, pink_bitness_t bitness, int arg, char *dest, size_t len)
{
	long addr;

	assert(arg >= 0 && arg < MAX_ARGS);

	if (!pink_util_get_arg(pid, bitness, arg, &addr))
		return false;

	return pink_util_movestr(pid, addr, dest, len);
}

char *
pink_decode_string_persistent(pid_t pid, pink_bitness_t bitness, int arg)
{
	long addr;

	assert(arg >= 0 && arg < MAX_ARGS);

	if (!pink_util_get_arg(pid, bitness, arg, &addr))
		return false;

	return pink_util_movestr_persistent(pid, addr);
}

bool
pink_encode_simple(pid_t pid, pink_bitness_t bitness, int arg, const void *src, size_t len)
{
	long addr;

	assert(arg >= 0 && arg < MAX_ARGS);

	if (!pink_util_get_arg(pid, bitness, arg, &addr))
		return false;

	return pink_util_putn(pid, addr, src, len);
}

bool
pink_encode_simple_safe(pid_t pid, pink_bitness_t bitness, int arg, const void *src, size_t len)
{
	long addr;

	assert(arg >= 0 && arg < MAX_ARGS);

	if (!pink_util_get_arg(pid, bitness, arg, &addr))
		return false;

	return pink_util_putn_safe(pid, addr, src, len);
}

bool
pink_decode_socket_call(pid_t pid, pink_bitness_t bitness, long *call, bool *decoded)
{
	assert(call != NULL);

	/* Decode socketcall(2) */
	if (!pink_util_get_arg(pid, bitness, 0, call))
		return false;

	if (decoded)
		*decoded = true;

	return true;
}

bool
pink_decode_socket_fd(pid_t pid, pink_bitness_t bitness, int arg, long *fd)
{
	long args;

	/* Decode socketcall(2) */
	if (!pink_util_get_arg(pid, bitness, 1, &args))
		return false;
	args += arg * sizeof(unsigned int);

	return pink_util_move(pid, args, fd);

}

pink_sockaddr_t *
pink_decode_socket_address(pid_t pid, pink_bitness_t bitness, int arg, long *fd)
{
	unsigned int iaddr, iaddrlen;
	long addr, addrlen, args;

	assert(arg >= 0 && arg < MAX_ARGS);

	/* Decode socketcall(2) */
	if (!pink_util_get_arg(pink, bitness, 1, &args))
		return false;
	if (fd && !pink_util_move(pid, args, fd))
		return NULL;
	args += arg * sizeof(unsigned int);
	if (!pink_util_move(pid, args, &iaddr))
		return NULL;
	args += sizeof(unsigned int);
	if (!pink_util_move(pid, args, &iaddrlen))
		return NULL;
	addr = iaddr;
	addrlen = iaddrlen;

	return pink_internal_decode_socket_address(pid, addr, addrlen);
}
