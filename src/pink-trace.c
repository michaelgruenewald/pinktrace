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
#include <sys/types.h>

#include <pinktrace/gcc.h>
#include <pinktrace/internal.h>
#include <pinktrace/trace.h>

inline bool
pink_trace_me(void)
{
	return !(0 > ptrace(PTRACE_TRACEME, 0, NULL, NULL));
}

inline bool
pink_trace_cont(pid_t pid, int sig)
{
	return !(0 > ptrace(PTRACE_CONT, pid, NULL, sig));
}

inline bool
pink_trace_kill(pid_t pid)
{
	return !(0 > ptrace(PTRACE_KILL, pid, NULL, NULL));
}

inline bool
pink_trace_singlestep(pid_t pid, int sig)
{
	return !(0 > ptrace(PTRACE_SINGLESTEP, pid, NULL, sig));
}

inline bool
pink_trace_syscall(pid_t pid, int sig)
{
	return !(0 > ptrace(PTRACE_SYSCALL, pid, NULL, sig));
}

inline bool
pink_trace_geteventmsg(pid_t pid, unsigned long *data)
{
	return !(0 > ptrace(PTRACE_GETEVENTMSG, pid, NULL, data));
}

bool
pink_trace_setup(pid_t pid, int options)
{
	int ptrace_options;

	ptrace_options = 0;
	if (options & PINK_TRACE_OPTION_SYSGOOD)
		ptrace_options |= PTRACE_O_TRACESYSGOOD;
	if (options & PINK_TRACE_OPTION_FORK)
		ptrace_options |= PTRACE_O_TRACEFORK;
	if (options & PINK_TRACE_OPTION_VFORK)
		ptrace_options |= PTRACE_O_TRACEVFORK;
	if (options & PINK_TRACE_OPTION_CLONE)
		ptrace_options |= PTRACE_O_TRACECLONE;
	if (options & PINK_TRACE_OPTION_EXEC)
		ptrace_options |= PTRACE_O_TRACEEXEC;
	if (options & PINK_TRACE_OPTION_VFORK_DONE)
		ptrace_options |= PTRACE_O_TRACEVFORKDONE;
	if (options & PINK_TRACE_OPTION_EXIT)
		ptrace_options |= PTRACE_O_TRACEEXIT;

	return !(0 > ptrace(PTRACE_SETOPTIONS, pid, NULL, ptrace_options));
}

inline bool
pink_trace_attach(pid_t pid)
{
	return !(0 > ptrace(PTRACE_ATTACH, pid, NULL, NULL));
}

inline bool
pink_trace_detach(pid_t pid, int sig)
{
	return !(0 > ptrace(PTRACE_DETACH, pid, NULL, sig));
}
