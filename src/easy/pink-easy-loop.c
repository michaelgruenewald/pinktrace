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

static pink_easy_tribool_t
pink_easy_loop_handle_callback(pink_easy_context_t *ctx, pink_easy_process_t *current, short flags)
{
	bool dummy;

	if (current && flags & PINK_EASY_CALLBACK_ESRCH) {
		/* Received ECHILD from the current process */
		dummy = pink_easy_process_tree_remove(ctx->tree, current->pid);
		assert(dummy);
		/* R.I.P. */
		if (ctx->tbl->cb_death)
			ctx->tbl->cb_death(ctx, current);
		if (current->destroy && current->data)
			current->destroy(current->data);
		free(current);
		return PINK_EASY_TRIBOOL_TRUE;
	}

	if (flags & PINK_EASY_CALLBACK_ABORT) {
		ctx->error = PINK_EASY_ERROR_CALLBACK_ABORT, ctx->fatal = true;
		return PINK_EASY_TRIBOOL_FALSE;
	}

	return PINK_EASY_TRIBOOL_NONE;
}

static bool
pink_easy_loop_handle_stop(pink_easy_context_t *ctx, pid_t pid)
{
	bool dummy;
	short val;
	pink_easy_tribool_t trb;
	pink_easy_errback_return_t ret;
	pink_easy_process_t *proc, *pproc;

	/* Step 1: Check if the process is born prematurely and add it to the
	 * process tree if required.
	 */
	proc = pink_easy_process_tree_search(ctx->tree, pid);
	if (!proc) {
		/* Stupid child was born before PTRACE_EVENT_FORK! */
		proc = calloc(1, sizeof(pink_easy_process_t));
		if (!proc) {
			ctx->error = PINK_EASY_ERROR_ALLOC_PREMATURE, ctx->fatal = true;
			if (ctx->tbl->eb_main)
				ctx->tbl->eb_main(ctx, pid);
			return false;
		}

		proc->pid = pid;
		if ((proc->bitness = pink_bitness_get(pid)) == PINK_BITNESS_UNKNOWN) {
			ctx->error = PINK_EASY_ERROR_BITNESS_PREMATURE, ctx->fatal = false;
			if (!ctx->tbl->eb_main)
				return -ctx->error;
			ret = ctx->tbl->eb_main(ctx, pid);
			/* We ignore PINK_EASY_ERRBACK_KILL
			 * here, because we haven't added this
			 * process to the tree yet.
			 */
			if (ret != PINK_EASY_ERRBACK_IGNORE && ret != PINK_EASY_ERRBACK_KILL)
				return -ctx->error;
		}
		proc->flags |= PINK_EASY_PROCESS_SUSPENDED;

		dummy = pink_easy_process_tree_insert(ctx->tree, proc);
		assert(dummy);

		if (!pink_trace_setup(proc->pid, ctx->options)) {
			ctx->error = PINK_EASY_ERROR_SETUP, ctx->fatal = false;
			if (!ctx->tbl->eb_main)
				return -ctx->error;
			ret = ctx->tbl->eb_main(ctx, proc);
			switch (ret) {
			case PINK_EASY_ERRBACK_ABORT:
				return false;
			case PINK_EASY_ERRBACK_KILL:
				dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
				assert(dummy);
				/* R.I.P. */
				if (ctx->tbl->cb_death)
					ctx->tbl->cb_death(ctx, proc);
				if (proc->destroy && proc->data)
					proc->destroy(proc->data);
				free(proc);
				return true;
			case PINK_EASY_ERRBACK_IGNORE:
				break;
			default:
				abort();
			}
		}

		/* Note: We don't call birth callback here, because we have no
		 * information about her parent.
		 *
		 * if (ctx->tbl->cb_birth)
		 *	ctx->tbl->cb_birth(ctx, proc, WTF);
		 */
	}
	else {
		pproc = pink_easy_process_tree_search(ctx->tree, proc->ppid);
		assert(pproc != NULL);

		if (ctx->tbl->cb_birth)
			ctx->tbl->cb_birth(ctx, proc, pproc);
	}

	/* Step 2: Call the "event_stop" callback and abort if requested. */
	if (ctx->tbl->cb_event_stop) {
		val = ctx->tbl->cb_event_stop(ctx, proc, proc->flags & PINK_EASY_PROCESS_SUSPENDED);
		if ((trb = pink_easy_loop_handle_callback(ctx, proc, val)) != PINK_EASY_TRIBOOL_NONE)
			return (bool)trb;
	}

	/* Step 3: Push the child to move in case it's not suspended. */
	if (!(proc->flags & PINK_EASY_PROCESS_SUSPENDED) && !pink_trace_syscall(proc->pid, 0)) {
		ctx->error = PINK_EASY_ERROR_STEP_STOP, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}

	return true;
}

static bool
pink_easy_loop_handle_syscall(pink_easy_context_t *ctx, pid_t pid)
{
	bool dummy;
	short val;
	pink_easy_tribool_t trb;
	pink_easy_errback_return_t ret;
	pink_easy_process_t *proc;

	/* Step 1: Search the process tree using the given process ID. */
	proc = pink_easy_process_tree_search(ctx->tree, pid);
	assert(proc != NULL);

	/* Step 2: Call the "event_syscall" callback and abort if requested. */
	if (ctx->tbl->cb_event_syscall) {
		val = ctx->tbl->cb_event_syscall(ctx, proc, !(proc->flags & PINK_EASY_PROCESS_INSYSCALL));
		if ((trb = pink_easy_loop_handle_callback(ctx, proc, val)) != PINK_EASY_TRIBOOL_NONE)
			return (bool)trb;
	}

	proc->flags ^= PINK_EASY_PROCESS_INSYSCALL;

	if (!pink_trace_syscall(proc->pid, 0)) {
		ctx->error = PINK_EASY_ERROR_STEP_SYSCALL, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}

	return true;
}

static bool
pink_easy_loop_handle_fork(pink_easy_context_t *ctx, pid_t pid, pink_event_t event)
{
	bool dummy, cpush, push;
	short val;
	unsigned long cpid;
	pink_easy_errback_return_t ret;
	pink_easy_process_t *proc, *cproc;
	pink_easy_callback_event_fork_t forkfunc;

	/* Step 1: Search the process tree using the given process ID. */
	proc = pink_easy_process_tree_search(ctx->tree, pid);
	assert(proc != NULL);

	/* Step 2: Figure out the process ID of the new-born child. */
	if (!pink_trace_geteventmsg(proc->pid, &cpid)) {
		ctx->error = PINK_EASY_ERROR_GETEVENTMSG_FORK, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			return true;
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			return true;
		default:
			abort();
		}
		/* never reached */
		assert(0);
	}

	/* Step 3: Search the process tree for the new-born child. */
	cproc = pink_easy_process_tree_search(ctx->tree, (pid_t)cpid);
	if (!cproc) {
		/* Child was not born yet */
		cproc = calloc(1, sizeof(pink_easy_process_t));
		if (!cproc) {
			ctx->error = PINK_EASY_ERROR_ALLOC_FORK, ctx->fatal = true;
			if (ctx->tbl->eb_main)
				ctx->tbl->eb_main(ctx, proc, cpid);
			return false;
		}

		cproc->pid = (pid_t)cpid;
		cproc->ppid = proc->pid;
		cproc->bitness = proc->bitness;
		cproc->flags = proc->flags;

		dummy = pink_easy_process_tree_insert(ctx->tree, cproc);
		assert(dummy);
	}
	else {
		/* Child was born before PTRACE_EVENT_FORK */
		cproc->ppid = proc->pid;
		if (ctx->tbl->cb_birth)
			ctx->tbl->cb_birth(ctx, cproc, proc);
	}

	/* Step 4: Run the "event_(fork|vfork|clone)" callback and abort if
	 * needed.
	 */
	cpush = push = true;
	forkfunc = (event == PINK_EVENT_FORK)
		? ctx->tbl->cb_event_fork
		: ((event == PINK_EVENT_VFORK)
				? ctx->tbl->cb_event_vfork
				: ctx->tbl->cb_event_clone);
	if (forkfunc) {
		val = forkfunc(ctx, proc, cproc, cproc->flags & PINK_EASY_PROCESS_SUSPENDED);
		if (val & PINK_EASY_CALLBACK_ESRCH_CHILD) {
			/* Received ECHILD from the new-born child */
			dummy = pink_easy_process_tree_remove(ctx->tree, cproc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, cproc);
			if (cproc->destroy && cproc->data)
				cproc->destroy(cproc->data);
			free(cproc);
			cpush = false;
		}
		if (val & PINK_EASY_CALLBACK_ESRCH) {
			/* Received ECHILD from the process */
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			push = false;
		}
		if (val & PINK_EASY_CALLBACK_ABORT) {
			ctx->error = PINK_EASY_ERROR_CALLBACK_ABORT, ctx->fatal = true;
			return false;
		}
	}

	if (!cpush && !push)
		return true;
	else if (!cpush)
		goto skip;

	/* Step 5: Resume child in case she was born prematurely. */
	if (cproc->flags & PINK_EASY_PROCESS_SUSPENDED && !pink_trace_syscall(cproc->pid, 0)) {
		ctx->error = PINK_EASY_ERROR_STEP_PREMATURE, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, cproc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, cproc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, cproc);
			if (cproc->destroy && cproc->data)
				cproc->destroy(cproc->data);
			free(cproc);
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}

skip:
	/* Step 6: Resume the parent as well */
	if (!pink_trace_syscall(proc->pid, 0)) {
		ctx->error = PINK_EASY_ERROR_STEP_FORK, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}

	return true;
}

static bool
pink_easy_loop_handle_exec(pink_easy_context_t *ctx, pid_t pid)
{
	bool dummy;
	short val;
	pink_bitness_t old_bitness;
	pink_easy_tribool_t trb;
	pink_easy_errback_return_t ret;
	pink_easy_process_t *proc;

	/* Step 1: Find the process in the process tree. */
	proc = pink_easy_process_tree_search(ctx->tree, pid);
	assert(proc != NULL);

	/* Step 2: Update bitness */
	old_bitness = proc->bitness;
	if ((proc->bitness = pink_bitness_get(proc->pid)) == PINK_BITNESS_UNKNOWN) {
		ctx->error = PINK_EASY_ERROR_BITNESS, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}

	/* Step 3: Run the "event_exec" callback and abort if needed */
	if (ctx->tbl->cb_event_exec)  {
		val = ctx->tbl->cb_event_exec(ctx, proc, old_bitness);
		if ((trb = pink_easy_loop_handle_callback(ctx, proc, val)) != PINK_EASY_TRIBOOL_NONE)
			return (bool)trb;
	}

	/* Step 4: Push the child to move! */
	if (!pink_trace_syscall(proc->pid, 0)) {
		ctx->error = PINK_EASY_ERROR_STEP_EXEC, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}

	return true;
}

static bool
pink_easy_loop_handle_exit(pink_easy_context_t *ctx, pid_t pid)
{
	bool dummy;
	short val;
	unsigned long code;
	pink_easy_tribool_t trb;
	pink_easy_errback_return_t ret;
	pink_easy_process_t *proc;

	/* Step 1: Find the process in the process tree. */
	proc = pink_easy_process_tree_search(ctx->tree, pid);
	assert(proc != NULL);

	/* Step 2: Get the exit status, call the "event_exit" callback and
	 * abort if requested.
	 */
	if (ctx->tbl->cb_event_exit) {
		if (!pink_trace_geteventmsg(proc->pid, &code)) {
			ctx->error = PINK_EASY_ERROR_GETEVENTMSG_FORK, ctx->fatal = false;
			if (!ctx->tbl->eb_main)
				return false;
			ret = ctx->tbl->eb_main(ctx, proc);
			switch (ret) {
			case PINK_EASY_ERRBACK_ABORT:
				return false;
			case PINK_EASY_ERRBACK_KILL:
				dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
				assert(dummy);
				/* R.I.P. */
				if (ctx->tbl->cb_death)
					ctx->tbl->cb_death(ctx, proc);
				if (proc->destroy && proc->data)
					proc->destroy(proc->data);
				free(proc);
				goto next;
			case PINK_EASY_ERRBACK_IGNORE:
				goto next;
			default:
				abort();
			}
		}

		val = ctx->tbl->cb_event_exit(ctx, proc, code);
		if ((trb = pink_easy_loop_handle_callback(ctx, proc, val)) != PINK_EASY_TRIBOOL_NONE)
			return (bool)trb;
	}

next:
	/* Step 3: Push the child so she will exit genuinely! */
	if (!pink_trace_syscall(proc->pid, 0)) {
		ctx->error = PINK_EASY_ERROR_STEP_EXIT, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}

	return true;
}

static bool
pink_easy_loop_handle_exit_genuine(pink_easy_context_t *ctx, pid_t pid, int status)
{
	bool dummy;
	short val;
	pink_easy_process_t *proc;

	/* Step 1: Find the process in the process tree */
	proc = pink_easy_process_tree_search(ctx->tree, pid);
	assert(proc != NULL);

	/* Step 2: Remove her from the process tree */
	dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
	assert(dummy);

	/* Step 3: Call the (exit|exit_signal) callback and abort if requested. */
	if (WIFSIGNALED(status)) {
		if (ctx->tbl->cb_exit_signal) {
			val = ctx->tbl->cb_exit_signal(ctx, proc, WTERMSIG(status));
			/* The ESRCH flags don't make any sense here,
			 * since the process has already died.
			 */
			if (val & PINK_EASY_CALLBACK_ABORT) {
				ctx->error = PINK_EASY_ERROR_CALLBACK_ABORT, ctx->fatal = true;
				return false;
			}
		}
	}
	else if (ctx->tbl->cb_exit) {
		val = ctx->tbl->cb_exit(ctx, proc, WEXITSTATUS(status));
		/* The ESRCH flags don't make any sense here,
		 * since the process has already died.
		 */
		if (val & PINK_EASY_CALLBACK_ABORT) {
			ctx->error = PINK_EASY_ERROR_CALLBACK_ABORT, ctx->fatal = true;
			return false;
		}
	}

	/* R.I.P. */
	if (ctx->tbl->cb_death)
		ctx->tbl->cb_death(ctx, proc);

	if (proc->destroy && proc->data)
		proc->destroy(proc->data);
	free(proc);

	return true;
}

static bool
pink_easy_loop_handle_genuine(pink_easy_context_t *ctx, pid_t pid, int status, bool unknown)
{
	bool dummy;
	short val;
	int stopsig;
	pink_easy_tribool_t trb;
	pink_easy_errback_return_t ret;
	pink_easy_process_t *proc;

	/* Step 1: Find the process in the process tree */
	proc = pink_easy_process_tree_search(ctx->tree, pid);
	assert(proc != NULL);

	stopsig = 0;
	if (unknown && !WIFSTOPPED(status)) {
		ctx->error = PINK_EASY_ERROR_EVENT_UNKNOWN, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			return true;
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			goto next;
		default:
			abort();
		}
	}

	stopsig = WSTOPSIG(status);

	if (ctx->tbl->cb_event_genuine) {
		val = ctx->tbl->cb_event_genuine(ctx, proc, stopsig);
		if ((trb = pink_easy_loop_handle_callback(ctx, proc, val)) != PINK_EASY_TRIBOOL_NONE)
			return (bool)trb;
	}

next:
	if (!pink_trace_syscall(proc->pid, stopsig)) {
		ctx->error = PINK_EASY_ERROR_STEP_GENUINE, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			return false;
		ret = ctx->tbl->eb_main(ctx, proc);
		switch (ret) {
		case PINK_EASY_ERRBACK_ABORT:
			return false;
		case PINK_EASY_ERRBACK_KILL:
			dummy = pink_easy_process_tree_remove(ctx->tree, proc->pid);
			assert(dummy);
			/* R.I.P. */
			if (ctx->tbl->cb_death)
				ctx->tbl->cb_death(ctx, proc);
			if (proc->destroy && proc->data)
				proc->destroy(proc->data);
			free(proc);
			return true;
			break;
		case PINK_EASY_ERRBACK_IGNORE:
			break;
		default:
			abort();
		}
	}
	return true;
}

int
pink_easy_loop(pink_easy_context_t *ctx)
{
	int status;
	pid_t wpid;
	pink_event_t event;

	assert(ctx != NULL);
	assert(ctx->tree != NULL);

	/* Enter the event loop */
	for (;;) {
		/* Wait for children */
		if ((wpid = pink_easy_internal_wait(&status)) < 0) {
			if (errno == ECHILD && ctx->tbl->cb_end)
				return ctx->tbl->cb_end(ctx, true);

			ctx->error = PINK_EASY_ERROR_WAIT;
			if (!ctx->tbl->eb_main)
				return -ctx->error;

			if (ctx->tbl->eb_main)
				ctx->tbl->eb_main(ctx);
			return -ctx->error;
		}

		/* Decide the event */
		event = pink_event_decide(status);

		switch (event) {
		case PINK_EVENT_STOP:
			if (!pink_easy_loop_handle_stop(ctx, wpid))
				return -ctx->error;
			break;
		case PINK_EVENT_SYSCALL:
			if (!pink_easy_loop_handle_syscall(ctx, wpid))
				return -ctx->error;
			break;
		case PINK_EVENT_FORK:
		case PINK_EVENT_VFORK:
		case PINK_EVENT_CLONE:
			if (!pink_easy_loop_handle_fork(ctx, wpid, event))
				return -ctx->error;
			break;
		case PINK_EVENT_EXEC:
			if (!pink_easy_loop_handle_exec(ctx, wpid))
				return -ctx->error;
			break;
		case PINK_EVENT_EXIT:
			if (!pink_easy_loop_handle_exit(ctx, wpid))
				return -ctx->error;
			break;
		case PINK_EVENT_EXIT_GENUINE:
		case PINK_EVENT_EXIT_SIGNAL:
			if (!pink_easy_loop_handle_exit_genuine(ctx, wpid, status))
				return -ctx->error;
			if (!ctx->tree->count)
				return ctx->tbl->cb_end ? ctx->tbl->cb_end(ctx, false) : 0;
			break;
		case PINK_EVENT_GENUINE:
		case PINK_EVENT_UNKNOWN:
			if (!pink_easy_loop_handle_genuine(ctx, wpid, status, event == PINK_EVENT_UNKNOWN))
				return -ctx->error;
			break;
		default:
			abort();
		}
	}

	return 0;
}
