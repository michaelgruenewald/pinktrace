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
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <pinktrace/internal.h>
#include <pinktrace/gcc.h>
#include <pinktrace/context.h>
#include <pinktrace/error.h>
#include <pinktrace/event.h>
#include <pinktrace/fork.h>
#include <pinktrace/loop.h>
#include <pinktrace/trace.h>

#define PINK_STEP_ONE(single, pid, sig)			\
	((single)					\
		? pink_trace_singlestep((pid), (sig))	\
		: pink_trace_syscall((pid), (sig)))

pink_noreturn
static int
pink_loop_attach(pink_unused pink_context_t *ctx)
{
	/* Not implemented yet */
	assert(false);
}

static int
pink_loop_fork(pink_context_t *ctx, pink_child_func_t func, void *userdata)
{
	bool followfork, issingle;
	int status;
	pid_t pid, wpid;
	enum pink_event event;

	assert(ctx != NULL);
	assert(ctx->handler != NULL);
	assert(func != NULL);

	followfork = ctx->options &
		(PINK_TRACE_OPTION_FORK | PINK_TRACE_OPTION_VFORK | PINK_TRACE_OPTION_CLONE);
	issingle = (ctx->step == PINK_STEP_SINGLE);

	if ((pid = pink_fork(ctx)) < 0)
		return -1;
	else if (!pid) /* child */
		_exit((func)(userdata));
	else { /* parent */
		waitpid(pid, &status, 0);

		assert(WIFSTOPPED(status));
		assert(WSTOPSIG(status) == SIGSTOP);

		/* After this point we never kill the children of this process.
		 * The eldest child has been saved to ctx->eldest by
		 * pink_fork() so the caller can do it herself.
		 */

		if (!PINK_STEP_ONE(issingle, pid, 0)) {
			ctx->error = PINK_ERROR_STEP;
			return -1;
		}

		for (;;) {
			wpid = followfork
				? waitpid(-1, &status, __WALL)
				: waitpid(pid, &status, 0);
			if (wpid < 0) {
				ctx->error = PINK_ERROR_WAIT;
				return -1;
			}

			/* Decide the event */
			event = pink_event_decide(ctx, event);
			/* Call the handler */
			if ((ctx->handler)(ctx, event, wpid) < 0) {
				ctx->error = PINK_ERROR_HANDLER;
				return -1;
			}
		}
		/* never reached */
		assert(false);
	}
	/* never reached */
	assert(false);
}

int
pink_loop(pink_context_t *ctx, pink_child_func_t func, void *userdata)
{
	return ctx->attach
		? pink_loop_attach(ctx)
		: pink_loop_fork(ctx, func, userdata);
}
