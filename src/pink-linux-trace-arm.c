/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon strace which is:
 *   Copyright (c) 1991, 1992 Paul Kranenburg <pk@cs.few.eur.nl>
 *   Copyright (c) 1993 Branko Lankester <branko@hacktic.nl>
 *   Copyright (c) 1993, 1994, 1995, 1996 Rick Sladkey <jrs@world.std.com>
 *   Copyright (c) 1996-1999 Wichert Akkerman <wichert@cistron.nl>
 *   Copyright (c) 1999 IBM Deutschland Entwicklung GmbH, IBM Corporation
 *                       Linux for s390 port by D.J. Barrow
 *                      <barrow_dj@mail.yahoo.com,djbarrow@de.ibm.com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <assert.h>
#include <errno.h>

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
	int save_errno;
	long scno;
	struct pt_regs regs;

	/*
	 * FIXME: Reading all registers is inefficient but we don't keep state.
	 */
	if (!pink_util_get_regs(pid, &regs))
		return false;

	/*
	 * Note: we only deal with only 32-bit CPUs here.
	 */
	if (regs.ARM_cpsr & 0x20) {
		/*
		 * Get the Thumb-mode system call number
		 */
		scno = regs.ARM_r7;
	}
	else {
		/*
		 * Get the ARM-mode system call number
		 */
		save_errno = errno;
		errno = 0;
		scno = ptrace(PTRACE_PEEKTEXT, pid, (void *)(regs.ARM_pc - 4), NULL);
		if (errno)
			return false;
		errno = save_errno;

		if (scno == 0) {
			*res = 0;
			return true;
		}

		/*
		 * Handle the EABI syscall convention.  We do not bother
		 * converting structures between the two ABIs, but basic
		 * functionality should work even if strace and the traced
		 * program have different ABIs.
		 */
		if (scno == 0xef000000)
			scno = regs.ARM_r7;
		else {
			if ((scno & 0x0ff00000) != 0x0f900000) {
				/* fprintf(stderr, "syscall: unknown syscall trap 0x%08lx\n", scno); */
				errno = EINVAL; /* FIXME: Return a unique errno here, not set by ptrace. */
				return false;
			}

			/*
			 * Fixup the syscall number
			 */
			scno &= 0x000fffff;
		}
	}

	if (scno & 0x0f0000)
		scno &= 0x0000ffff;

	*res = scno;
	return true;
}

bool
pink_util_set_syscall(pid_t pid, pink_unused pink_bitness_t bitness, long scno)
{
	struct pt_regs regs;

	if (!pink_util_get_regs(pid, &regs))
		return false;

	/*
	 * FIXME: This doesn't handle ARM-mode system call numbers
	 */
	regs.ARM_r7 = scno;

	return pink_util_set_regs(pid, &regs);
}

bool
pink_util_get_return(pid_t pid, long *res)
{
	struct pt_regs regs;

	assert(res != NULL);

	if (!pink_util_get_regs(pid, &regs))
		return false;

	*res = regs.ARM_r0;
	return true;
}

bool
pink_util_set_return(pid_t pid, long ret)
{
	struct pt_regs regs;

	if (!pink_util_get_regs(pid, &regs))
		return false;

	regs.ARM_r0 = ret;

	return pink_util_set_regs(pid, &regs);
}

bool
pink_util_get_arg(pid_t pid, pink_unused pink_bitness_t bitness, unsigned ind, long *res)
{
	struct pt_regs regs;

	assert(ind < PINK_MAX_INDEX);
	assert(res != NULL);

	if (!pink_util_get_regs(pid, &regs))
		return false;

	*res = regs.uregs[ind];
	return true;
}

bool
pink_decode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, void *dest, size_t len)
{
	long addr;

	return pink_util_get_arg(pid, bitness, ind, &addr) && pink_util_moven(pid, addr, dest, len);
}

bool
pink_decode_string(pid_t pid, pink_bitness_t bitness, unsigned ind, char *dest, size_t len)
{
	long addr;

	return pink_util_get_arg(pid, bitness, ind, &addr) && pink_util_movestr(pid, addr, dest, len);
}

char *
pink_decode_string_persistent(pid_t pid, pink_bitness_t bitness, unsigned ind)
{
	long addr;

	if (pink_unlikely(!pink_util_get_arg(pid, bitness, ind, &addr)))
		return NULL;

	return pink_util_movestr_persistent(pid, addr);
}

bool
pink_encode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, const void *src, size_t len)
{
	long addr;

	return pink_util_get_arg(pid, bitness, ind, &addr) && pink_util_putn(pid, addr, src, len);
}

bool
pink_encode_simple_safe(pid_t pid, pink_bitness_t bitness, unsigned ind, const void *src, size_t len)
{
	long addr;

	return pink_util_get_arg(pid, bitness, ind, &addr) && pink_util_putn_safe(pid, addr, src, len);
}

bool
pink_has_socketcall(pink_unused pink_bitness_t bitness)
{
	return false;
}

bool
pink_decode_socket_call(pid_t pid, pink_bitness_t bitness, long *subcall)
{
	/* No decoding needed */
	return pink_util_get_syscall(pid, bitness, subcall);
}

bool
pink_decode_socket_fd(pid_t pid, pink_bitness_t bitness, unsigned ind, long *fd)
{
	/* No decoding needed */
	return pink_util_get_arg(pid, bitness, ind, fd);
}

bool
pink_decode_socket_address(pid_t pid, pink_bitness_t bitness, unsigned ind, long *fd, pink_socket_address_t *paddr)
{
	long addr, addrlen;

	assert(paddr != NULL);

	/* No decoding needed */
	if (pink_unlikely(fd && !pink_util_get_arg(pid, bitness, 0, fd)))
		return false;
	if (pink_unlikely(!pink_util_get_arg(pid, bitness, ind, &addr)))
		return false;
	if (pink_unlikely(!pink_util_get_arg(pid, bitness, ind + 1, &addrlen)))
		return false;

	return pink_internal_decode_socket_address(pid, addr, addrlen, paddr);
}
