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

#if defined(I386) || defined(IA64) || defined(POWERPC) || defined(POWERPC64)
const char *
pink_name_syscall(long scno, pink_unused pink_bitness_t bitness)
{
#if defined(PINKTRACE_FREEBSD)
#if defined(I386)
	return pink_name_syscall_i386(scno);
#else
#error unsupported architecture
#endif /* I386 */
#elif defined(PINKTRACE_LINUX)
	return pink_name_syscall_nobitness(scno);
#else
#error unsupported operating system
#endif /* PINKTRACE_FREEBSD */
}
#elif defined(X86_64)
const char *
pink_name_syscall(long scno, pink_bitness_t bitness)
{
#if defined(PINKTRACE_FREEBSD)
	return pink_name_syscall_amd64(scno, bitness);
#elif defined(PINKTRACE_LINUX)
	return (bitness == PINK_BITNESS_32)
		? pink_name_syscall32(scno)
		: pink_name_syscall64(scno);
#else
#error unsupported operating system
#endif /* PINKTRACE_FREEBSD */
}
#else
#error unsupported architecture
#endif
