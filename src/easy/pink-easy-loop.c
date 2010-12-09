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
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

int
pink_easy_loop(pink_easy_context_t *ctx)
{
	bool followfork, ret;
	short cbret;
	int status, wopt;
	unsigned long code, npid;
	pid_t pid, wpid;
	pink_bitness_t old_bitness;
	pink_event_t event;
	pink_easy_process_t *proc, *nproc;
	pink_easy_callback_event_fork_t cbfork;

	assert(ctx != NULL);
	assert(ctx->tree != NULL);
	assert(ctx->eldest != NULL);
	assert(ctx->eldest->flags & PINK_EASY_PROCESS_STARTUP);

#define CALL_ERROR(p, v)				\
	do {						\
		ctx->error = (v);			\
		if (ctx->tbl->cb_error)			\
			ctx->tbl->cb_error(ctx, (p));	\
		return -(v);				\
	} while (0)

	followfork = ctx->eldest->flags & PINK_EASY_PROCESS_FOLLOWFORK;
	pid = followfork ? -1 : ctx->eldest->pid;
#ifdef __WALL
	wopt = followfork ? __WALL : 0;
#else
	wopt = 0;
#endif /* __WALL */

	/* Push the child to move! */
	if (!pink_trace_syscall(ctx->eldest->pid, 0))
		CALL_ERROR(ctx->eldest, PINK_EASY_ERROR_STEP_INITIAL);

	/* Startup completed */
	ctx->eldest->flags &= ~PINK_EASY_PROCESS_STARTUP;

	/* Enter the event loop */
	for (;;) {
		/* Wait for children */
		if ((wpid = waitpid(pid, &status, wopt)) < 0) {
			if (errno == ECHILD && ctx->tbl->cb_end)
				ctx->tbl->cb_end(ctx, true);
			else if (!ctx->tbl->cb_error) {
				ctx->error = (pid < 0) ? PINK_EASY_ERROR_WAIT_ALL : PINK_EASY_ERROR_WAIT;
				return -ctx->error;
			}

			if (pid < 0) {
				/* Waiting for all process IDs */
				CALL_ERROR(NULL, PINK_EASY_ERROR_WAIT_ALL);
			}
			else {
				/* Waiting for just one process ID */
				proc = pink_easy_process_tree_search(ctx->tree, pid);
				CALL_ERROR(proc, PINK_EASY_ERROR_WAIT);
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
				proc = calloc(1, sizeof(pink_easy_process_t));
				if (!proc)
					CALL_ERROR(NULL, PINK_EASY_ERROR_ALLOC_PREMATURE);

				proc->pid = wpid;
				if ((proc->bitness = pink_bitness_get(wpid)) == PINK_BITNESS_UNKNOWN) {
					if (proc->destroy && proc->data)
						proc->destroy(proc->data);
					free(proc);
					CALL_ERROR(NULL, PINK_EASY_ERROR_BITNESS_PREMATURE);
				}
				proc->flags |= PINK_EASY_PROCESS_SUSPENDED;
				if (followfork)
					proc->flags |= PINK_EASY_PROCESS_FOLLOWFORK;

				ret = pink_easy_process_tree_insert(ctx->tree, proc);
				assert(ret);
				/* Note: We don't call birth callback here,
				 * because we have no information about her
				 * parent.
				 *
				 * if (ctx->tbl->cb_birth)
				 *	ctx->tbl->cb_birth(ctx, proc, WTF);
				 */
			}

			if (ctx->tbl->cb_event_stop) {
				cbret = ctx->tbl->cb_event_stop(ctx, proc, proc->flags & PINK_EASY_PROCESS_SUSPENDED);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			if (!(proc->flags & PINK_EASY_PROCESS_SUSPENDED) && !pink_trace_syscall(proc->pid, 0))
				CALL_ERROR(proc, PINK_EASY_ERROR_STEP_STOP);

			break;
		case PINK_EVENT_SYSCALL:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (ctx->tbl->cb_event_syscall) {
				cbret = ctx->tbl->cb_event_syscall(ctx, proc, !(proc->flags & PINK_EASY_PROCESS_INSYSCALL));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			proc->flags ^= PINK_EASY_PROCESS_INSYSCALL;

			if (!pink_trace_syscall(proc->pid, 0))
				CALL_ERROR(proc, PINK_EASY_ERROR_STEP_SYSCALL);

			break;
		case PINK_EVENT_FORK:
		case PINK_EVENT_VFORK:
		case PINK_EVENT_CLONE:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (!pink_trace_geteventmsg(proc->pid, &npid))
				CALL_ERROR(proc, PINK_EASY_ERROR_GETEVENTMSG_FORK);

			nproc = pink_easy_process_tree_search(ctx->tree, npid);
			if (!nproc) {
				/* Child was not born yet */
				nproc = calloc(1, sizeof(pink_easy_process_t));
				if (!nproc) {
					/* TODO: Maybe we need a way to
					 * pass npid to the errback? */
					CALL_ERROR(NULL, PINK_EASY_ERROR_ALLOC);
				}

				nproc->pid = (pid_t)npid;
				nproc->bitness = proc->bitness;
				nproc->flags = proc->flags;

				ret = pink_easy_process_tree_insert(ctx->tree, nproc);
				assert(ret);
				if (ctx->tbl->cb_birth)
					ctx->tbl->cb_birth(ctx, nproc, proc);
			}
			else {
				/* Child was born before PTRACE_EVENT_FORK but
				 * the tbl->cb_birth callback was not called. */
				if (ctx->tbl->cb_birth)
					ctx->tbl->cb_birth(ctx, nproc, proc);
			}

			cbfork = (event == PINK_EVENT_FORK)
				? ctx->tbl->cb_event_fork
				: ((event == PINK_EVENT_VFORK)
						? ctx->tbl->cb_event_vfork
						: ctx->tbl->cb_event_clone);
			if (cbfork) {
				cbret = cbfork(ctx, proc, nproc, nproc->flags & PINK_EASY_PROCESS_SUSPENDED);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			/* Resume child in case she was born prematurely. */
			if (nproc->flags & PINK_EASY_PROCESS_SUSPENDED && !pink_trace_syscall(nproc->pid, 0))
				CALL_ERROR(nproc, PINK_EASY_ERROR_STEP_PREMATURE);

			/* Resume the parent as well */
			if (!pink_trace_syscall(proc->pid, 0))
				CALL_ERROR(proc, PINK_EASY_ERROR_STEP_FORK);

			break;
		case PINK_EVENT_EXEC:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			old_bitness = proc->bitness;
			if ((proc->bitness = pink_bitness_get(proc->pid)) == PINK_BITNESS_UNKNOWN)
				CALL_ERROR(proc, PINK_EASY_ERROR_BITNESS);

			if (ctx->tbl->cb_event_exec)  {
				cbret = ctx->tbl->cb_event_exec(ctx, proc, old_bitness);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			if (!pink_trace_syscall(proc->pid, 0))
				CALL_ERROR(proc, PINK_EASY_ERROR_STEP_EXEC);

			break;
		case PINK_EVENT_EXIT:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (ctx->tbl->cb_event_exit) {
				if (!pink_trace_geteventmsg(proc->pid, &code))
					CALL_ERROR(proc, PINK_EASY_ERROR_GETEVENTMSG_EXIT);

				cbret = ctx->tbl->cb_event_exit(ctx, proc, code);
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			if (!pink_trace_syscall(proc->pid, 0))
				CALL_ERROR(proc, PINK_EASY_ERROR_STEP_EXIT);

			break;
		case PINK_EVENT_EXIT_GENUINE:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			ret = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(ret);

			if (ctx->tbl->cb_exit) {
				cbret = ctx->tbl->cb_exit(ctx, proc, WEXITSTATUS(status));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);

			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);

			if (ctx->tree->count == 0) {
				if (ctx->tbl->cb_end)
					ctx->tbl->cb_end(ctx, false);
				else
					return 0;
			}

			break;
		case PINK_EVENT_EXIT_SIGNAL:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			ret = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(ret);

			if (ctx->tbl->cb_exit_signal) {
				cbret = ctx->tbl->cb_exit_signal(ctx, proc, WTERMSIG(status));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);

			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);

			if (ctx->tree->count == 0) {
				if (ctx->tbl->cb_end)
					ctx->tbl->cb_end(ctx, false);
				else
					return 0;
			}

			break;
		case PINK_EVENT_GENUINE:
		case PINK_EVENT_UNKNOWN:
			proc = pink_easy_process_tree_search(ctx->tree, wpid);
			assert(proc != NULL);

			if (event == PINK_EVENT_UNKNOWN && !WIFSTOPPED(status))
				CALL_ERROR(proc, PINK_EASY_ERROR_EVENT_UNKNOWN);

			if (ctx->tbl->cb_event_genuine) {
				cbret = ctx->tbl->cb_event_genuine(ctx, proc, WSTOPSIG(status));
				if (cbret & PINK_EASY_CALLBACK_ABORT)
					CALL_ERROR(proc, PINK_EASY_ERROR_CALLBACK_ABORT);
			}

			if (!pink_trace_syscall(proc->pid, WSTOPSIG(status)))
				CALL_ERROR(proc, PINK_EASY_ERROR_STEP_GENUINE);

			break;

		default:
			abort();
		}
	}

	return 0;
}
