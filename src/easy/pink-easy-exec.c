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

enum {
	PINK_INTERNAL_FUNC_EXECVE,
	PINK_INTERNAL_FUNC_EXECV,
	PINK_INTERNAL_FUNC_EXECVP,
};

static int
pink_easy_exec_helper(pink_easy_context_t *ctx, int type, const char *filename, char *const argv[], char *const envp[])
{
	bool ret;
	int status;

	assert(ctx != NULL);
	assert(ctx->tree != NULL);

#define CALL_ERROR(p, v)				\
	do {						\
		ctx->error = (v);			\
		if (ctx->tbl->cb_error)			\
			ctx->tbl->cb_error(ctx, (p)); 	\
		return -(v);				\
	} while (0)

	ctx->eldest = calloc(1, sizeof(pink_easy_process_t));
	if (!ctx->eldest)
		CALL_ERROR(NULL, PINK_EASY_ERROR_ALLOC_ELDEST);

	ret = pink_easy_process_tree_insert(ctx->tree, ctx->eldest);
	assert(ret);

	if ((ctx->eldest->pid = vfork()) < 0)
		CALL_ERROR(NULL, PINK_EASY_ERROR_VFORK);
	else if (!ctx->eldest->pid) { /* child */
		if (!pink_trace_me())
			_exit(ctx->tbl->cb_cerror ? ctx->tbl->cb_cerror(PINK_EASY_CHILD_ERROR_SETUP) : EXIT_FAILURE);
		switch (type) {
		case PINK_INTERNAL_FUNC_EXECVE:
			execve(filename, argv, envp);
		case PINK_INTERNAL_FUNC_EXECV:
			execv(filename, argv);
		case PINK_INTERNAL_FUNC_EXECVP:
			execvp(filename, argv);
		default:
			abort();
		}
		/* execve() failed */
		_exit(ctx->tbl->cb_cerror ? ctx->tbl->cb_cerror(PINK_EASY_CHILD_ERROR_EXEC) : EXIT_FAILURE);
	}
	/* parent */

	/* Wait for the initial SIGTRAP */
	waitpid(ctx->eldest->pid, &status, 0);
	assert(WIFSTOPPED(status));
	assert(WSTOPSIG(status) == SIGTRAP);

	/* Figure out bitness */
	if ((ctx->eldest->bitness = pink_bitness_get(ctx->eldest->pid)) == PINK_BITNESS_UNKNOWN)
		CALL_ERROR(ctx->eldest, PINK_EASY_ERROR_BITNESS_ELDEST);

	/* Set up tracing options */
	if (!pink_trace_setup(ctx->eldest->pid, ctx->options))
		CALL_ERROR(ctx->eldest, PINK_EASY_ERROR_SETUP_ELDEST);

	/* Set up flags */
	ctx->eldest->flags |= PINK_EASY_PROCESS_STARTUP;
	if (ctx->options & PINK_TRACE_OPTION_FORK
			|| ctx->options & PINK_TRACE_OPTION_VFORK
			|| ctx->options & PINK_TRACE_OPTION_CLONE)
		ctx->eldest->flags |= PINK_EASY_PROCESS_FOLLOWFORK;

	/* Happy birthday! */
	if (ctx->tbl->cb_birth)
		ctx->tbl->cb_birth(ctx, ctx->eldest, NULL);

	return pink_easy_loop(ctx);

#undef CALL_ERROR
}

int
pink_easy_execve(pink_easy_context_t *ctx, const char *filename, char *const argv[], char *const envp[])
{
	return pink_easy_exec_helper(ctx, PINK_INTERNAL_FUNC_EXECVE, filename, argv, envp);
}

int
pink_easy_execv(pink_easy_context_t *ctx, const char *path, char *const argv[])
{
	return pink_easy_exec_helper(ctx, PINK_INTERNAL_FUNC_EXECV, path, argv, NULL);
}

int
pink_easy_execvp(pink_easy_context_t *ctx, const char *file, char *const argv[])
{
	return pink_easy_exec_helper(ctx, PINK_INTERNAL_FUNC_EXECVP, file, argv, NULL);
}
