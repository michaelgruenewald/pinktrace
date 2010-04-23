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
#include <stdlib.h>
#include <unistd.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

pink_noreturn
static int
pink_loop_attach(pink_unused pink_event_handler_t *ctx)
{
	/* Not implemented yet */
	abort();
}

static int
pink_loop_fork(pink_event_handler_t *handler, pink_child_func_t func, void *userdata)
{
	bool followfork, issingle, ret;
	int status;
	pid_t pid, wpid;
	pink_event_t event;
	pink_error_return_t errback;

	assert(func != NULL);

	followfork = handler->ctx->options &
		(PINK_TRACE_OPTION_FORK | PINK_TRACE_OPTION_VFORK | PINK_TRACE_OPTION_CLONE);
	issingle = (handler->ctx->step == PINK_STEP_SINGLE);

	if ((pid = pink_fork(handler->ctx)) < 0)
		return -1;
	else if (!pid) /* child */
		_exit(func(userdata));
	else { /* parent */
		/* After this point we never kill the children of this process.
		 * The eldest child has been saved to handler->ctx->eldest by
		 * pink_fork() so the caller can do it herself.
		 */

		/* Again after this point we always try to call the error
		 * handler in case of errors, so the caller can clean up and do
		 * other things she may want in case of errors.
		 */
		ret = false;

		if (!(issingle
			? pink_trace_singlestep(pid, 0)
			: pink_trace_syscall(pid, 0))) {
			handler->ctx->error = PINK_ERROR_STEP;
			errback = handler->cb_error(handler->ctx, pid, handler->userdata_error);
			if (errback.fatal)
				return errback.code;
		}

		wpid = pid;
		for (;;) {
			wpid = followfork
				? waitpid(-1, &status, __WALL)
				: waitpid(wpid, &status, 0);
			if (wpid < 0) {
				handler->ctx->error = PINK_ERROR_WAIT;
				errback = handler->cb_error(handler->ctx, wpid, handler->userdata_error);
				if (errback.fatal)
					return errback.code;
			}

			/* Decide the event */
			event = pink_event_decide(handler->ctx, status);

			/* Call the handler */
			switch (event) {
			case PINK_EVENT_STOP:
				ret = handler->cb_stop
					? handler->cb_stop(handler->ctx, wpid,
						WSTOPSIG(status), handler->userdata_stop)
					: pink_callback_signal_default(handler->ctx, wpid,
						WSTOPSIG(status), NULL);
				break;
			case PINK_EVENT_SYSCALL:
				ret = handler->cb_syscall
					? handler->cb_syscall(handler->ctx, wpid,
						handler->userdata_syscall)
					: pink_callback_syscall_default(handler->ctx, wpid,
						NULL);
				break;
			case PINK_EVENT_FORK:
				ret = handler->cb_fork(handler->ctx, wpid,
					handler->userdata_fork);
				break;
			case PINK_EVENT_VFORK:
				ret = handler->cb_vfork(handler->ctx, wpid,
					handler->userdata_vfork);
				break;
			case PINK_EVENT_CLONE:
				ret = handler->cb_clone(handler->ctx, wpid,
					handler->userdata_clone);
				break;
			case PINK_EVENT_EXEC:
				ret = handler->cb_exec(handler->ctx, wpid,
					handler->userdata_exec);
				break;
			case PINK_EVENT_VFORK_DONE:
				ret = handler->cb_vfork_done(handler->ctx, wpid,
					handler->userdata_vfork_done);
				break;
			case PINK_EVENT_EXIT:
				ret = handler->cb_exit(handler->ctx, wpid,
					handler->userdata_exit);
				break;
			case PINK_EVENT_GENUINE:
				ret = handler->cb_genuine
					? handler->cb_genuine(handler->ctx, wpid,
						WSTOPSIG(status), handler->userdata_genuine)
					: pink_callback_signal_default(handler->ctx, wpid,
						WSTOPSIG(status), NULL);
				break;
			case PINK_EVENT_EXIT_GENUINE:
				ret = handler->cb_exit_genuine
					? handler->cb_exit_genuine(handler->ctx, wpid,
						WEXITSTATUS(status), handler->userdata_exit_genuine)
					: pink_callback_exit_default(handler->ctx, wpid,
						WEXITSTATUS(status), NULL);
				break;
			case PINK_EVENT_EXIT_SIGNAL:
				ret = handler->cb_exit_signal
					? handler->cb_exit_signal(handler->ctx, wpid,
						WTERMSIG(status), handler->userdata_exit_signal)
					: pink_callback_exit_default(handler->ctx, wpid,
						WTERMSIG(status), (void *)1);
				break;
			case PINK_EVENT_UNKNOWN:
			default:
				ret = handler->cb_unknown
					? handler->cb_unknown(handler->ctx, wpid,
						WSTOPSIG(status), handler->userdata_unknown)
					: pink_callback_signal_default(handler->ctx, wpid,
						WSTOPSIG(status), NULL);
				break;
			}

			if (!ret) {
				/* Eeek! Handler returned error!
				 */
				handler->ctx->error = PINK_ERROR_HANDLER;
				errback = handler->cb_error(handler->ctx, wpid, handler->userdata_error);
				if (errback.fatal)
					return errback.code;
			}
		}
		/* never reached */
		assert(false);
	}
	/* never reached */
	assert(false);
}

int
pink_loop(pink_event_handler_t *handler, pink_child_func_t func, void *userdata)
{
	assert(handler != NULL);
	assert(handler->ctx != NULL);

	/* Set the tracing options depending on the handlers. */
	if (handler->cb_fork != NULL)
		handler->ctx->options |= PINK_TRACE_OPTION_FORK;
	else
		handler->ctx->options &= ~PINK_TRACE_OPTION_FORK;

	if (handler->cb_vfork != NULL)
		handler->ctx->options |= PINK_TRACE_OPTION_VFORK;
	else
		handler->ctx->options &= ~PINK_TRACE_OPTION_VFORK;

	if (handler->cb_clone != NULL)
		handler->ctx->options |= PINK_TRACE_OPTION_CLONE;
	else
		handler->ctx->options &= ~PINK_TRACE_OPTION_CLONE;

	if (handler->cb_exec != NULL)
		handler->ctx->options |= PINK_TRACE_OPTION_EXEC;
	else
		handler->ctx->options &= ~PINK_TRACE_OPTION_EXEC;

	if (handler->cb_vfork_done != NULL)
		handler->ctx->options |= PINK_TRACE_OPTION_VFORK_DONE;
	else
		handler->ctx->options &= ~PINK_TRACE_OPTION_VFORK_DONE;

	if (handler->cb_exit != NULL)
		handler->ctx->options |= PINK_TRACE_OPTION_EXIT;
	else
		handler->ctx->options &= ~PINK_TRACE_OPTION_EXIT;

	return handler->ctx->attach
		? pink_loop_attach(handler)
		: pink_loop_fork(handler, func, userdata);
}
