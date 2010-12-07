/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
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

#include <sys/types.h>
#include <sys/wait.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

pink_easy_error_t
pink_easy_loop(pink_easy_context_t *ctx)
{
	bool followfork, ret;
	short cbret;
	int status, wopt;
	unsigned long code, npid;
	pid_t pid, wpid;
	pink_bitness_t orig_bitness;
	pink_event_t event;
	pink_easy_process_t *proc, *nproc;
	short (*cbfork) (pink_easy_context_t *, pink_easy_process_t *, pink_easy_process_t *, bool);

	assert(ctx != NULL);
	assert(ctx->tree != NULL);
	assert(ctx->eldest != NULL);
	assert(ctx->eldest->flags & PINK_EASY_PROCESS_STARTUP);

	followfork = ctx->eldest->flags & PINK_EASY_PROCESS_FOLLOWFORK;
	pid = followfork ? -1 : ctx->eldest->pid;
#ifdef __WALL
	wopt = followfork ? __WALL : 0;
#else
	wopt = 0;
#endif /* __WALL */

	/* Push the child to move! */
	if (!pink_trace_syscall(ctx->eldest->pid, 0)) {
		if (ctx->cb->eb_main)
			ctx->cb->eb_main(ctx, ctx->eldest, PINK_EASY_ERROR_STEP_INITIAL);
		return PINK_EASY_ERROR_STEP_INITIAL;
	}

	/* Startup completed */
	ctx->eldest->flags &= ~PINK_EASY_PROCESS_STARTUP;

	/* Enter the event loop */
	for (;;) {
		/* Wait for children */
		if ((wpid = waitpid(pid, &status, wopt)) < 0) {
			if (!ctx->cb->eb_main)
				return (pid < 0) ? PINK_EASY_ERROR_WAIT_ALL : PINK_EASY_ERROR_WAIT;

			if (pid < 0) {
				/* Waiting for all process IDs */
				ctx->cb->eb_main(ctx, NULL, PINK_EASY_ERROR_WAIT_ALL);
				return PINK_EASY_ERROR_WAIT_ALL;
			}
			else {
				/* Waiting for just one process ID */
				proc = pink_easy_process_tree_search(ctx->tree, pid);
				ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_WAIT);
				return PINK_EASY_ERROR_WAIT;
			}
		}

		/* Decide the event */
		event = pink_event_decide(status);

		switch (event) {
		case PINK_EVENT_STOP:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			if (!proc) {
				/* Stupid child was born before
				 * PTRACE_EVENT_FORK! */
				proc = malloc(sizeof(pink_easy_process_t));
				if (!proc) {
					if (ctx->cb->eb_main)
						ctx->cb->eb_main(ctx, NULL, PINK_EASY_ERROR_MALLOC_PREMATURE_CHILD);
					return PINK_EASY_ERROR_MALLOC_PREMATURE_CHILD;
				}

				proc->pid = wpid;
				if ((proc->bitness = pink_bitness_get(wpid)) == PINK_BITNESS_UNKNOWN) {
					if (ctx->cb->eb_main)
						ctx->cb->eb_main(ctx, NULL, PINK_EASY_ERROR_BITNESS_PREMATURE_CHILD);
					free(proc);
					return PINK_EASY_ERROR_BITNESS_PREMATURE_CHILD;
				}
				proc->flags = 0;
				proc->flags |= PINK_EASY_PROCESS_SUSPENDED;
				if (followfork)
					proc->flags |= PINK_EASY_PROCESS_FOLLOWFORK;
				proc->data = NULL;

				ret = pink_easy_process_tree_insert(ctx->tree, proc);
				assert(ret);
			}

			if (ctx->cb->cb_stop) {
				cbret = ctx->cb->cb_stop(ctx, proc, proc->flags & PINK_EASY_PROCESS_SUSPENDED);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			if (!(proc->flags & PINK_EASY_PROCESS_SUSPENDED) && !pink_trace_syscall(proc->pid, 0)) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_STEP_AFTER_STOP);
				return PINK_EASY_ERROR_STEP_AFTER_STOP;
			}

			break;
		case PINK_EVENT_SYSCALL:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (ctx->cb->cb_syscall) {
				cbret = ctx->cb->cb_syscall(ctx, proc, !(proc->flags & PINK_EASY_PROCESS_INSYSCALL));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			proc->flags ^= PINK_EASY_PROCESS_INSYSCALL;

			if (!pink_trace_syscall(proc->pid, 0)) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_STEP_AFTER_SYSCALL);
				return PINK_EASY_ERROR_STEP_AFTER_SYSCALL;
			}

			break;
		case PINK_EVENT_FORK:
		case PINK_EVENT_VFORK:
		case PINK_EVENT_CLONE:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (!pink_trace_geteventmsg(proc->pid, &npid)) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_GETEVENTMSG_FORK);
				return PINK_EASY_ERROR_GETEVENTMSG_FORK;
			}

			nproc = pink_easy_process_tree_search(ctx->tree, npid);
			if (!nproc) {
				/* Child was not born yet */
				nproc = malloc(sizeof(pink_easy_process_t));
				if (!nproc) {
					if (ctx->cb->eb_main) {
						/* TODO: Maybe we need a way to
						 * pass npid to the errback? */
						ctx->cb->eb_main(ctx, NULL, PINK_EASY_ERROR_MALLOC_NEW_CHILD);
					}
					return PINK_EASY_ERROR_MALLOC_NEW_CHILD;
				}

				nproc->pid = (pid_t)npid;
				nproc->bitness = proc->bitness;
				nproc->flags = proc->flags;

				ret = pink_easy_process_tree_insert(ctx->tree, nproc);
				assert(ret);
			}

			cbfork = (event == PINK_EVENT_FORK)
				? ctx->cb->cb_fork
				: ((event == PINK_EVENT_VFORK)
						? ctx->cb->cb_vfork
						: ctx->cb->cb_clone);

			if (cbfork) {
				cbret = cbfork(ctx, proc, nproc, nproc->flags & PINK_EASY_PROCESS_SUSPENDED);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			/* Resume child in case she was born prematurely. */
			if (nproc->flags & PINK_EASY_PROCESS_SUSPENDED && !pink_trace_syscall(nproc->pid, 0)) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, nproc, PINK_EASY_ERROR_STEP_PREMATURE);
				return PINK_EASY_ERROR_STEP_PREMATURE;
			}

			/* Resume the parent as well */
			if (!pink_trace_syscall(proc->pid, 0)) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_STEP_AFTER_FORK);
				return PINK_EASY_ERROR_STEP_AFTER_FORK;
			}

			break;
		case PINK_EVENT_EXEC:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			orig_bitness = proc->bitness;
			if ((proc->bitness = pink_bitness_get(proc->pid)) == PINK_BITNESS_UNKNOWN) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_BITNESS_EXEC);
				return PINK_EASY_ERROR_BITNESS_EXEC;
			}

			if (ctx->cb->cb_exec) {
				cbret = ctx->cb->cb_exec(ctx, proc, orig_bitness);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			if (!pink_trace_syscall(proc->pid, 0)) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_STEP_AFTER_EXEC);
				return PINK_EASY_ERROR_STEP_AFTER_EXEC;
			}

			break;
		case PINK_EVENT_EXIT:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (ctx->cb->cb_exit) {
				if (!pink_trace_geteventmsg(proc->pid, &code)) {
					if (ctx->cb->eb_main)
						ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_GETEVENTMSG_EXIT);
					return PINK_EASY_ERROR_GETEVENTMSG_EXIT;
				}

				cbret = ctx->cb->cb_exit(ctx, proc, code);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			break;
		case PINK_EVENT_EXIT_GENUINE:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			ret = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(ret);

			if (ctx->cb->cb_exit_genuine) {
				cbret = ctx->cb->cb_exit_genuine(ctx, proc, WEXITSTATUS(status));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			free(proc);

			if (ctx->tree->count == 0)
				return PINK_EASY_ERROR_SUCCESS;

			break;
		case PINK_EVENT_EXIT_SIGNAL:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			ret = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(ret);

			if (ctx->cb->cb_exit_signal) {
				cbret = ctx->cb->cb_exit_signal(ctx, proc, WTERMSIG(status));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			free(proc);

			if (ctx->tree->count == 0)
				return PINK_EASY_ERROR_SUCCESS;

			break;
		case PINK_EVENT_GENUINE:
		case PINK_EVENT_UNKNOWN:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (event == PINK_EVENT_UNKNOWN && !WIFSTOPPED(status)) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_EVENT_UNKNOWN);
				return PINK_EASY_ERROR_EVENT_UNKNOWN;
			}

			if (ctx->cb->cb_genuine) {
				cbret = ctx->cb->cb_genuine(ctx, proc, WSTOPSIG(status));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					return PINK_EASY_ERROR_CALLBACK_ABORT;
			}

			if (!pink_trace_syscall(proc->pid, WSTOPSIG(status))) {
				if (ctx->cb->eb_main)
					ctx->cb->eb_main(ctx, proc, PINK_EASY_ERROR_STEP_AFTER_GENUINE);
				return PINK_EASY_ERROR_STEP_AFTER_GENUINE;
			}

			break;

		default:
			abort();
		}
	}

	return PINK_EASY_ERROR_SUCCESS;
}
