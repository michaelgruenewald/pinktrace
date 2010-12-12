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
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

int
pink_easy_call(pink_easy_context_t *ctx, pink_easy_child_func_t func, void *userdata)
{
	bool dummy;
	int status;
	pid_t pid;
	pink_easy_errback_return_t ret;
	pink_easy_process_t *proc;

	assert(ctx != NULL);
	assert(ctx->tree != NULL);
	assert(func != NULL);

	proc = calloc(1, sizeof(pink_easy_process_t));
	if (!proc) {
		ctx->error = PINK_EASY_ERROR_ALLOC_ELDEST, ctx->fatal = true;
		if (ctx->tbl->eb_main)
			ctx->tbl->eb_main(ctx);
		return -ctx->error;
	}

	if ((pid = fork()) < 0) {
		ctx->error = PINK_EASY_ERROR_FORK, ctx->fatal = true;
		if (ctx->tbl->eb_main)
			ctx->tbl->eb_main(ctx);
		goto fail;
	}
	else if (!pid) { /* child */
		if (!pink_trace_me())
			_exit(ctx->tbl->eb_child ? ctx->tbl->eb_child(PINK_EASY_CHILD_ERROR_SETUP) : EXIT_FAILURE);
		kill(getpid(), SIGSTOP);
		_exit(func(userdata));
	}
	/* parent */

	/* Wait for the initial SIGSTOP */
	if (pink_easy_internal_wait(&status) < 0) {
		ctx->error = PINK_EASY_ERROR_WAIT_ELDEST, ctx->fatal = true;
		if (ctx->tbl->eb_main)
			ctx->tbl->eb_main(ctx, pid);
		goto fail;
	}
	if (!WIFSTOPPED(status) || WSTOPSIG(status) != SIGSTOP) {
		ctx->error = PINK_EASY_ERROR_SIGNAL_INITIAL, ctx->fatal = true;
		if (ctx->tbl->eb_main)
			ctx->tbl->eb_main(ctx, pid, status);
		goto fail;
	}

	/* Set up tracing options */
	if (!pink_trace_setup(pid, ctx->options)) {
		ctx->error = PINK_EASY_ERROR_SETUP_ELDEST, ctx->fatal = true;
		if (ctx->tbl->eb_main)
			ctx->tbl->eb_main(ctx, pid);
		goto fail;
	}

	/* Figure out bitness */
	if ((proc->bitness = pink_bitness_get(pid)) == PINK_BITNESS_UNKNOWN) {
		ctx->error = PINK_EASY_ERROR_BITNESS_ELDEST, ctx->fatal = false;
		if (!ctx->tbl->eb_main)
			goto fail;
		ret = ctx->tbl->eb_main(ctx, pid);
		if (ret != PINK_EASY_ERRBACK_IGNORE)
			goto fail;
		/* Killing eldest child and resuming is not possible,
		 * so we ignore PINK_EASY_ERRBACK_KILL.
		 */
	}

	/* Set up flags */
	proc->flags |= PINK_EASY_PROCESS_STARTUP;
	if (ctx->options & PINK_TRACE_OPTION_FORK
			|| ctx->options & PINK_TRACE_OPTION_VFORK
			|| ctx->options & PINK_TRACE_OPTION_CLONE)
		proc->flags |= PINK_EASY_PROCESS_FOLLOWFORK;

	/* Insert the process into the tree */
	proc->pid = pid;
	dummy = pink_easy_process_tree_insert(ctx->tree, proc);
	assert(dummy);

	/* Keep a reference of the eldest child */
	ctx->eldest = proc;

	/* Happy birthday! */
	if (ctx->tbl->cb_birth)
		ctx->tbl->cb_birth(ctx, proc, NULL);

	return pink_easy_loop(ctx);
fail:
	free(proc);
	return -ctx->error;
}
