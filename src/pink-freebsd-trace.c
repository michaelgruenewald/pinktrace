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

#include <errno.h>
#include <stdbool.h>
#include <sys/types.h>
#include <stdio.h> /* NULL */

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

bool
pink_trace_me(void)
{
	return !(0 > ptrace(PT_TRACE_ME, 0, NULL, 0));
}

bool
pink_trace_cont(pid_t pid, int sig, char *addr)
{
	return !(0 > ptrace(PT_CONTINUE, pid, addr, sig));
}

bool
pink_trace_kill(pid_t pid)
{
	return !(0 > ptrace(PT_KILL, pid, NULL, 0));
}

bool
pink_trace_singlestep(pid_t pid, int sig)
{
	return !(0 > ptrace(PT_STEP, pid, (caddr_t)1, sig));
}

bool
pink_trace_syscall(pid_t pid, int sig)
{
	return !(0 > ptrace(PT_SYSCALL, pid, (caddr_t)1, sig));
}

bool
pink_trace_attach(pid_t pid)
{
	return !(0 > ptrace(PT_ATTACH, pid, NULL, 0));
}

bool
pink_trace_detach(pid_t pid, int sig)
{
	return !(0 > ptrace(PT_DETACH, pid, (caddr_t)1, sig));
}
