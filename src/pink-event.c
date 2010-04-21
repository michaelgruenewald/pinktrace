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
#include <sys/wait.h>

#include <pinktrace/internal.h>
#include <pinktrace/event.h>
#include <pinktrace/context.h>
#include <pinktrace/trace.h>

pink_event_t
pink_event_decide(pink_context_t *ctx, int status)
{
	unsigned int event;

	/* TODO: Right now this function assumes PTRACE_O_TRACESYSGOOD is set.
	 * It might be a good idea to support event decisions when this flag
	 * isn't set.
	 */
	assert(ctx->options & PINK_TRACE_OPTION_SYSGOOD);

	if (WIFSTOPPED(status)) {
		switch (WSTOPSIG(status)) {
		case SIGSTOP:
			return PINK_EVENT_STOP;
		case SIGTRAP | 0x80:
			return PINK_EVENT_SYSCALL;
		default:
			event = (status >> 16) & 0xffff;
			switch (event) {
			case PTRACE_EVENT_FORK:
				return PINK_EVENT_FORK;
			case PTRACE_EVENT_VFORK:
				return PINK_EVENT_VFORK;
			case PTRACE_EVENT_CLONE:
				return PINK_EVENT_CLONE;
			case PTRACE_EVENT_VFORK_DONE:
				return PINK_EVENT_VFORK_DONE;
			case PTRACE_EVENT_EXEC:
				return PINK_EVENT_EXEC;
			case PTRACE_EVENT_EXIT:
				return PINK_EVENT_EXIT;
			default:
				return PINK_EVENT_GENUINE;
			}
		}
	}
	else if (WIFEXITED(status))
		return PINK_EVENT_EXIT_GENUINE;
	else if (WIFSIGNALED(status))
		return PINK_EVENT_EXIT_SIGNAL;

	return PINK_EVENT_UNKNOWN;
}

const char *
pink_event_tostring(pink_event_t event)
{
	switch (event) {
	case PINK_EVENT_STOP:
		return "stop";
	case PINK_EVENT_SYSCALL:
		return "syscall";
	case PINK_EVENT_FORK:
		return "fork";
	case PINK_EVENT_VFORK:
		return "vfork";
	case PINK_EVENT_CLONE:
		return "clone";
	case PINK_EVENT_VFORK_DONE:
		return "vfork_done";
	case PINK_EVENT_EXEC:
		return "exec";
	case PINK_EVENT_EXIT:
		return "exit";
	case PINK_EVENT_GENUINE:
		return "genuine";
	case PINK_EVENT_EXIT_GENUINE:
		return "exit_genuine";
	case PINK_EVENT_EXIT_SIGNAL:
		return "exit_signal";
	case PINK_EVENT_UNKNOWN:
	default:
		return "unknown";
	}
}
