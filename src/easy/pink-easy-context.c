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
#include <stdlib.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

static bool
pink_easy_process_tree_free_entry(pink_easy_process_t *proc, PINK_UNUSED void *userdata)
{
	if (proc) {
		if (proc->destroy && proc->data)
			proc->destroy(proc->data);
		free(proc);
	}
	return true;
}

pink_easy_context_t *
pink_easy_context_new(int options, void *data, pink_easy_free_func_t func)
{
	pink_easy_context_t *ctx;

	ctx = calloc(1, sizeof(pink_easy_context_t));
	if (!ctx)
		return NULL;

	ctx->tree = pink_easy_process_tree_new();
	if (!ctx->tree) {
		free(ctx);
		return NULL;
	}

	ctx->data = data;
	ctx->error = PINK_EASY_ERROR_SUCCESS;
	ctx->options = options;
	ctx->destroy = func;

	return ctx;
}

pink_easy_error_t
pink_easy_context_get_error(const pink_easy_context_t *ctx)
{
	return ctx->error;
}

void
pink_easy_context_clear_error(pink_easy_context_t *ctx)
{
	ctx->error = PINK_EASY_ERROR_SUCCESS;
}

void
pink_easy_context_destroy(pink_easy_context_t *ctx)
{
	assert(ctx != NULL);

	if (ctx->tree) {
		pink_easy_process_tree_walk(ctx->tree, pink_easy_process_tree_free_entry, NULL);
		free(ctx->tree);
	}

	if (ctx->destroy && ctx->data)
		ctx->destroy(ctx->data);

	free(ctx);
}

pink_easy_process_t *
pink_easy_context_get_eldest(const pink_easy_context_t *ctx)
{
	return ctx->eldest;
}

void *
pink_easy_context_get_data(const pink_easy_context_t *ctx)
{
	return ctx->data;
}

pink_easy_process_tree_t *
pink_easy_context_get_tree(const pink_easy_context_t *ctx)
{
	return ctx->tree;
}

void
pink_easy_context_set_callback_birth(pink_easy_context_t *ctx, pink_easy_callback_birth_t func)
{
	ctx->cb_birth = func;
}

void
pink_easy_context_set_callback_death(pink_easy_context_t *ctx, pink_easy_callback_death_t func)
{
	ctx->cb_death = func;
}

void
pink_easy_context_set_callback_end(pink_easy_context_t *ctx, pink_easy_callback_end_t func)
{
	ctx->cb_end = func;
}

void
pink_easy_context_set_callback_error(pink_easy_context_t *ctx, pink_easy_callback_error_t func)
{
	ctx->cb_error = func;
}

void
pink_easy_context_set_callback_cerror(pink_easy_context_t *ctx, pink_easy_callback_cerror_t func)
{
	ctx->cb_cerror = func;
}

void
pink_easy_context_set_callback_event_stop(pink_easy_context_t *ctx, pink_easy_callback_event_stop_t func)
{
	ctx->cb_event_stop = func;
}

void
pink_easy_context_set_callback_event_syscall(pink_easy_context_t *ctx, pink_easy_callback_event_syscall_t func)
{
	ctx->cb_event_syscall = func;
}

void
pink_easy_context_set_callback_event_fork(pink_easy_context_t *ctx, pink_easy_callback_event_fork_t func)
{
	ctx->cb_event_fork = func;
}

void
pink_easy_context_set_callback_event_vfork(pink_easy_context_t *ctx, pink_easy_callback_event_fork_t func)
{
	ctx->cb_event_vfork = func;
}

void
pink_easy_context_set_callback_event_clone(pink_easy_context_t *ctx, pink_easy_callback_event_fork_t func)
{
	ctx->cb_event_clone = func;
}

void
pink_easy_context_set_callback_event_exec(pink_easy_context_t *ctx, pink_easy_callback_event_exec_t func)
{
	ctx->cb_event_exec = func;
}

void
pink_easy_context_set_callback_event_exit(pink_easy_context_t *ctx, pink_easy_callback_event_exit_t func)
{
	ctx->cb_event_exit = func;
}

void
pink_easy_context_set_callback_event_genuine(pink_easy_context_t *ctx, pink_easy_callback_event_genuine_t func)
{
	ctx->cb_event_genuine = func;
}

void
pink_easy_context_set_callback_exit(pink_easy_context_t *ctx, pink_easy_callback_exit_t func)
{
	ctx->cb_exit = func;
}

void
pink_easy_context_set_callback_exit_signal(pink_easy_context_t *ctx, pink_easy_callback_exit_t func)
{
	ctx->cb_exit_signal = func;
}
