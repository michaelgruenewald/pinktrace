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
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <pinktrace/easy/pink.h>

static int
eb_child(pink_easy_child_error_t error)
{
	fprintf(stderr, "%s:%d: child[%i]: %s\n",
			__func__, __LINE__,
			getpid(), pink_easy_child_strerror(error));
	return -1;
}

static short
cb_pre_exit(PINK_UNUSED const pink_easy_context_t *ctx, PINK_UNUSED pink_easy_process_t *current, unsigned long status)
{
	short f;

	f = 0;
	if (WEXITSTATUS(status) != 127) {
		fprintf(stderr, "%s:%d: 127 != %i\n", __func__, __LINE__, WEXITSTATUS(status));
		f |= PINK_EASY_CFLAG_ABORT;
	}
	return f;
}

static int
exit_immediately_func(void *data)
{
	int code = *((int *)data);
	return code;
}

int
main(void)
{
	int cret, ret;
	pink_easy_error_t error;
	pink_easy_callback_table_t tbl;
	pink_easy_context_t *ctx;

	memset(&tbl, 0, sizeof(pink_easy_callback_table_t));
	tbl.cerror = eb_child;
	tbl.pre_exit = cb_pre_exit;

	ctx = pink_easy_context_new(PINK_TRACE_OPTION_SYSGOOD | PINK_TRACE_OPTION_EXIT, &tbl, NULL, NULL);
	if (!ctx) {
		perror("pink_easy_context_new");
		abort();
	}

	cret = 127;
	if ((ret = pink_easy_call(ctx, exit_immediately_func, &cret))) {
		fprintf(stderr, "%s:%d: pink_easy_call: %d %d(%s)\n",
				__func__, __LINE__,
				ret, errno, strerror(errno));
		abort();
	}
	pink_easy_loop(ctx);
	error = pink_easy_context_get_error(ctx);
	if (error != PINK_EASY_ERROR_SUCCESS) {
		fprintf(stderr, "%s:%d: %i (%s) != %i (%s) -> %d (%s)\n",
				__func__, __LINE__,
				error, pink_easy_strerror(error),
				PINK_EASY_ERROR_SUCCESS,
				pink_easy_strerror(PINK_EASY_ERROR_SUCCESS),
				errno, strerror(errno));
		abort();
	}

	pink_easy_context_clear_error(ctx);

	cret = 128;
	if ((ret = pink_easy_call(ctx, exit_immediately_func, &cret))) {
		fprintf(stderr, "%s:%d: pink_easy_call: %d %d(%s)\n",
				__func__, __LINE__,
				ret, errno, strerror(errno));
		abort();
	}
	pink_easy_loop(ctx);
	error = pink_easy_context_get_error(ctx);
	if (error != PINK_EASY_ERROR_CALLBACK_ABORT) {
		fprintf(stderr, "%s:%d: %i (%s) != %i (%s) -> %d (%s)\n",
				__func__, __LINE__,
				error, pink_easy_strerror(error),
				PINK_EASY_ERROR_CALLBACK_ABORT,
				pink_easy_strerror(PINK_EASY_ERROR_CALLBACK_ABORT),
				errno, strerror(errno));
		abort();
	}

	pink_easy_context_destroy(ctx);
	return 0;
}
