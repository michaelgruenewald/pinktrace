/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010, 2011 Ali Polatel <alip@exherbo.org>
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
#include <string.h>
#include <sys/queue.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

pink_easy_context_t *
pink_easy_context_new(int options, const pink_easy_callback_table_t *tbl, void *data, pink_easy_free_func_t func)
{
	pink_easy_context_t *ctx;

	ctx = calloc(1, sizeof(pink_easy_context_t));
	if (!ctx)
		return NULL;

	/* Callbacks */
	ctx->tbl = malloc(sizeof(pink_easy_callback_table_t));
	if (!ctx->tbl) {
		free(ctx);
		return NULL;
	}
	memcpy(ctx->tbl, tbl, sizeof(pink_easy_callback_table_t));

	/* Process list */
	SLIST_INIT(&ctx->process_list);

	/* User data */
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
	pink_easy_process_t *current;

	assert(ctx != NULL);

	if (ctx->destroy && ctx->data)
		ctx->destroy(ctx->data);

	SLIST_FOREACH(current, &ctx->process_list, entries) {
		if (current->destroy && current->data)
			current->destroy(current->data);
		free(current);
	}

	if (ctx->tbl)
		free(ctx->tbl);

	free(ctx);
}

void *
pink_easy_context_get_data(const pink_easy_context_t *ctx)
{
	return ctx->data;
}

pink_easy_process_list_t *
pink_easy_context_get_process_list(pink_easy_context_t *ctx)
{
	return &ctx->process_list;
}
