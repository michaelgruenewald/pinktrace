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

static inline int
pink_handle_error(const pink_context_t *ctx, pid_t pid)
{
	return (ctx->handler->ev_error != NULL)
		? (ctx->handler->ev_error)(ctx, pid)
		: -1;
}

static inline int
pink_handle_default(pink_context_t *ctx, bool single, pid_t pid, int sig)
{
	bool ret;
	int mret;

	mret = 0;
	ret = single ? pink_trace_singlestep(pid, sig) : pink_trace_syscall(pid, sig);
	if (!ret) {
		ctx->error = PINK_ERROR_STEP;
		if ((mret = pink_handle_error(ctx, pid)))
			return mret;
	}
	return 0;
}

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
	int ret, status;
	pid_t pid, wpid;
	pink_event_t event;

	assert(func != NULL);

	followfork = ctx->options &
		(PINK_TRACE_OPTION_FORK | PINK_TRACE_OPTION_VFORK | PINK_TRACE_OPTION_CLONE);
	issingle = (ctx->step == PINK_STEP_SINGLE);

	if ((pid = pink_fork(ctx)) < 0)
		return -1;
	else if (!pid) /* child */
		_exit((func)(userdata));
	else { /* parent */
		/* After this point we never kill the children of this process.
		 * The eldest child has been saved to ctx->eldest by
		 * pink_fork() so the caller can do it herself.
		 */

		/* Again after this point we always try to call the error
		 * handler in case of errors, so the caller can clean up and do
		 * other things she may want in case of errors.
		 */
		ret = 0;

		if ((ret = pink_handle_default(ctx, issingle, pid, 0)))
			return ret;

		for (;;) {
			wpid = followfork
				? waitpid(-1, &status, __WALL)
				: waitpid(pid, &status, 0);
			if (wpid < 0) {
				ctx->error = PINK_ERROR_WAIT;
				if ((ret = pink_handle_error(ctx, wpid)))
					return ret;
			}

			/* Decide the event */
			event = pink_event_decide(ctx, event);

			/* Call the handler */
			switch (event) {
			case PINK_EVENT_STOP:
				if (ctx->handler->ev_stop != NULL)
					ret = (ctx->handler->ev_stop)(ctx, wpid, WSTOPSIG(status));
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_SYSCALL:
				if (ctx->handler->ev_syscall != NULL)
					ret = (ctx->handler->ev_syscall)(ctx, wpid);
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_FORK:
				if (ctx->handler->ev_fork != NULL)
					ret = (ctx->handler->ev_fork)(ctx, wpid);
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_VFORK:
				if (ctx->handler->ev_vfork != NULL)
					ret = (ctx->handler->ev_vfork)(ctx, wpid);
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_CLONE:
				if (ctx->handler->ev_clone != NULL)
					ret = (ctx->handler->ev_clone)(ctx, wpid);
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_EXEC:
				if (ctx->handler->ev_exec != NULL)
					ret = (ctx->handler->ev_exec)(ctx, wpid);
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_VFORK_DONE:
				if (ctx->handler->ev_vfork_done != NULL)
					ret = (ctx->handler->ev_vfork_done)(ctx, wpid);
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_EXIT:
				if (ctx->handler->ev_exit != NULL)
					ret = (ctx->handler->ev_exit)(ctx, wpid);
				else if ((ret = pink_handle_default(ctx, issingle, wpid, 0)))
					return ret;
				break;
			case PINK_EVENT_GENUINE:
				if (ctx->handler->ev_genuine != NULL)
					ret = (ctx->handler->ev_genuine)(ctx, wpid, WSTOPSIG(status));
				else if ((ret = pink_handle_default(ctx, issingle, wpid, WSTOPSIG(status))))
					return ret;
				break;
			case PINK_EVENT_EXIT_GENUINE:
				if (ctx->handler->ev_exit_genuine != NULL)
					ret = (ctx->handler->ev_exit_genuine)(ctx, wpid, WEXITSTATUS(status));
				/* Check if the dead child was the eldest */
				if (wpid == ctx->eldest)
					return WEXITSTATUS(status);
				break;
			case PINK_EVENT_EXIT_SIGNAL:
				if (ctx->handler->ev_exit_signal != NULL)
					ret = (ctx->handler->ev_exit_signal)(ctx, wpid, WTERMSIG(status));
				/* Check if the dead child was the eldest */
				if (wpid == ctx->eldest)
					return 128 + WTERMSIG(status);
				break;
			case PINK_EVENT_UNKNOWN:
			default:
				if (ctx->handler->ev_unknown != NULL)
					ret = (ctx->handler->ev_unknown)(ctx, wpid, WSTOPSIG(status));
				else if ((ret = pink_handle_default(ctx, issingle, wpid, WSTOPSIG(status))))
					return ret;
				break;
			}

			if (ret) {
				/* Eeek! Handler returned non-zero!
				 */
				ctx->error = PINK_ERROR_HANDLER;
				if ((ret = pink_handle_error(ctx, wpid)))
					return ret;
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
	assert(ctx != NULL);
	assert(ctx->handler != NULL);

	/* Set the tracing options depending on the handlers. */
	if (ctx->handler->ev_fork != NULL)
		ctx->options |= PINK_TRACE_OPTION_FORK;
	else
		ctx->options &= ~PINK_TRACE_OPTION_FORK;

	if (ctx->handler->ev_vfork != NULL)
		ctx->options |= PINK_TRACE_OPTION_VFORK;
	else
		ctx->options &= ~PINK_TRACE_OPTION_VFORK;

	if (ctx->handler->ev_clone != NULL)
		ctx->options |= PINK_TRACE_OPTION_CLONE;
	else
		ctx->options &= ~PINK_TRACE_OPTION_CLONE;

	if (ctx->handler->ev_exec != NULL)
		ctx->options |= PINK_TRACE_OPTION_EXEC;
	else
		ctx->options &= ~PINK_TRACE_OPTION_EXEC;

	if (ctx->handler->ev_vfork_done != NULL)
		ctx->options |= PINK_TRACE_OPTION_VFORK_DONE;
	else
		ctx->options &= ~PINK_TRACE_OPTION_VFORK_DONE;

	if (ctx->handler->ev_exit != NULL)
		ctx->options |= PINK_TRACE_OPTION_EXIT;
	else
		ctx->options &= ~PINK_TRACE_OPTION_EXIT;

	return ctx->attach
		? pink_loop_attach(ctx)
		: pink_loop_fork(ctx, func, userdata);
}
