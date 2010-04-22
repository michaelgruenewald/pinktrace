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

#include <stdbool.h>
#include <stdlib.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

bool
pink_callback_signal_default(const pink_context_t *ctx, pid_t pid, int signum,
	pink_unused void *userdata)
{
	return (ctx->step == PINK_STEP_SINGLE)
		? pink_trace_singlestep(pid, signum)
		: pink_trace_syscall(pid, signum);
}

bool
pink_callback_syscall_default(const pink_context_t *ctx, pid_t pid,
	pink_unused void *userdata)
{
	return (ctx->step == PINK_STEP_SINGLE)
		? pink_trace_singlestep(pid, 0)
		: pink_trace_syscall(pid, 0);
}

bool
pink_callback_exit_default(const pink_context_t *ctx, pid_t pid, int excode, void *userdata)
{
	if (pid == ctx->eldest)
		exit(userdata ? 128 + excode : excode);
	return true;
}
