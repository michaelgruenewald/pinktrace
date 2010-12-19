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

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

struct _status {
	pink_easy_process_t *proc;
	int status;
};

/** Non-intrusive waitpid(-1) **/
static bool
pink_easy_internal_waiting_walk(pink_easy_process_t *proc, void *userdata)
{
	int status;
	pid_t pid;
	struct _status *s;

	pid = waitpid(proc->pid, &status, WNOHANG);
	if (!pid) {
		/* No change in state, go on */
		return true;
	}

	s = userdata;
	s->proc = proc;

	if (pid > 0) {
		/* Process changed state! */
		s->status = status;
		return false;
	}

	/* waitpid() error */
	return false;
}

pid_t
pink_easy_internal_wait(pink_easy_context_t *ctx, int *status)
{
	bool dummy;
	struct _status s;

	for (;;) {
		s.proc = NULL;
		s.status = 0;
		errno = 0;
		pink_easy_process_tree_walk(ctx->tree, pink_easy_internal_waiting_walk, &s);
		if (!s.proc)
			continue;
		if (errno) {
			if (errno != ECHILD)
				return -1;
			/* Don't die on me Mia! */
			dummy = pink_easy_process_tree_remove(ctx->tree, s.proc->pid);
			assert(dummy);

			/* R.I.P. */
			if (ctx->tbl->death)
				ctx->tbl->death(ctx, s.proc);
			if (s.proc->destroy && s.proc->data)
				s.proc->destroy(s.proc->data);
			free(s.proc);

			if (!ctx->tree->count) {
				errno = ECHILD;
				return -1;
			}

			continue;
		}
		break;
	}

	*status = s.status;
	return s.proc->pid;
}

/** Initialize tracing **/
int
pink_easy_internal_init(pink_easy_context_t *ctx, pink_easy_process_t *proc, int sig)
{
	bool dummy;
	int status;

	/* Wait for the initial sig */
	if (waitpid(proc->pid, &status, 0) < 0) {
		ctx->error = PINK_EASY_ERROR_WAIT_ELDEST;
		if (ctx->tbl->error)
			ctx->tbl->error(ctx, proc->pid);
		return -ctx->error;
	}
	if (!WIFSTOPPED(status) || WSTOPSIG(status) != sig) {
		ctx->error = PINK_EASY_ERROR_SIGNAL_INITIAL;
		if (ctx->tbl->error)
			ctx->tbl->error(ctx, proc->pid, status);
		return -ctx->error;
	}

	/* Set up tracing options */
	if (!pink_trace_setup(proc->pid, ctx->options)) {
		ctx->error = PINK_EASY_ERROR_SETUP_ELDEST;
		if (ctx->tbl->error)
			ctx->tbl->error(ctx, proc->pid);
		return -ctx->error;
	}

	/* Figure out bitness */
	if ((proc->bitness = pink_bitness_get(proc->pid)) == PINK_BITNESS_UNKNOWN) {
		ctx->error = PINK_EASY_ERROR_BITNESS_ELDEST;
		if (ctx->tbl->error)
			ctx->tbl->error(ctx, proc->pid);
		return -ctx->error;
	}

	/* Set up flags */
	proc->flags |= PINK_EASY_PROCESS_STARTUP;
	if (ctx->options & PINK_TRACE_OPTION_FORK
			|| ctx->options & PINK_TRACE_OPTION_VFORK
			|| ctx->options & PINK_TRACE_OPTION_CLONE)
		proc->flags |= PINK_EASY_PROCESS_FOLLOWFORK;

	/* Insert the process into the tree */
	proc->ppid = -1;
	proc->flags &= ~PINK_EASY_PROCESS_STARTUP;
	dummy = pink_easy_process_tree_insert(ctx->tree, proc);
	assert(dummy);

	/* Happy birthday! */
	if (ctx->tbl->birth)
		ctx->tbl->birth(ctx, proc, NULL);

	/* Push the child to move! */
	if (!pink_trace_syscall(proc->pid, 0)) {
		ctx->error = PINK_EASY_ERROR_STEP_INITIAL;
		if (ctx->tbl->error)
			ctx->tbl->error(ctx, proc);
		return -ctx->error;
	}

	return 0;
}
