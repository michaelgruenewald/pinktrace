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

#include "check_pinktrace_easy.h"

#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/pink.h>

static short
_cb_exit_genuine(PINK_UNUSED const pink_easy_context_t *ctx, PINK_UNUSED pink_easy_process_t *proc, int code)
{
	if (code != 127) {
		printf("%s: 127 != %i\n", __func__, code);
		return PINK_EASY_CALLBACK_ABORT;
	}
	return 0;
}

static int
_exit_immediately_func(void *data)
{
	int code = *((int *)data);
	return code;
}

START_TEST(t_loop_exit_genuine)
{
	int ret;
	pink_easy_error_t e;
	pink_easy_callback_table_t tbl;
	pink_easy_context_t *ctx;

	memset(&tbl, 0, sizeof(pink_easy_callback_table_t));
	tbl.cb_exit = _cb_exit_genuine;
	ctx = pink_easy_context_new(0, &tbl, NULL, NULL);
	fail_unless(ctx != NULL, "%d(%s)", errno, strerror(errno));

	ret = 127;
	pink_easy_call(ctx, _exit_immediately_func, &ret);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_SUCCESS, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_SUCCESS, errno, strerror(errno));

	ret = 128;
	pink_easy_call(ctx, _exit_immediately_func, &ret);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_CALLBACK_ABORT, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_CALLBACK_ABORT, errno, strerror(errno));

	pink_easy_context_destroy(ctx);
}
END_TEST

static short
_cb_exit_signal(PINK_UNUSED const pink_easy_context_t *ctx, PINK_UNUSED pink_easy_process_t *proc, int sig)
{
	if (sig != SIGTERM) {
		printf("%s: SIGTERM != %s\n", __func__, strsignal(sig));
		return PINK_EASY_CALLBACK_ABORT;
	}
	return 0;
}

static int
_signal_immediately_func(void *data)
{
	int sig = *((int *)data);
	kill(getpid(), sig);
	return 0;
}

START_TEST(t_loop_exit_signal)
{
	int sig;
	pink_easy_error_t e;
	pink_easy_callback_table_t tbl;
	pink_easy_context_t *ctx;

	memset(&tbl, 0, sizeof(pink_easy_callback_table_t));
	tbl.cb_exit_signal = _cb_exit_signal;
	ctx = pink_easy_context_new(PINK_TRACE_OPTION_SYSGOOD, &tbl, NULL, NULL);
	fail_unless(ctx != NULL, "%d(%s)", errno, strerror(errno));

	sig = SIGTERM;
	pink_easy_call(ctx, _signal_immediately_func, &sig);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_SUCCESS, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_SUCCESS, errno, strerror(errno));

	sig = SIGKILL;
	pink_easy_call(ctx, _signal_immediately_func, &sig);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_CALLBACK_ABORT, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_CALLBACK_ABORT, errno, strerror(errno));

	pink_easy_context_destroy(ctx);
}
END_TEST

static short
_cb_genuine(PINK_UNUSED const pink_easy_context_t *ctx, PINK_UNUSED pink_easy_process_t *proc, int sig)
{
	if (sig != SIGTTIN) {
		printf("%s: SIGTTIN != %s\n", __func__, strsignal(sig));
		return PINK_EASY_CALLBACK_ABORT;
	}
	return 0;
}

START_TEST(t_loop_genuine)
{
	int sig;
	pink_easy_error_t e;
	pink_easy_callback_table_t tbl;
	pink_easy_context_t *ctx;

	memset(&tbl, 0, sizeof(pink_easy_callback_table_t));
	tbl.cb_event_genuine = _cb_genuine;
	ctx = pink_easy_context_new(PINK_TRACE_OPTION_SYSGOOD, &tbl, NULL, NULL);
	fail_unless(ctx != NULL, "%d(%s)", errno, strerror(errno));

	sig = SIGTTIN;
	pink_easy_call(ctx, _signal_immediately_func, &sig);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_SUCCESS, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_SUCCESS, errno, strerror(errno));

	sig = SIGTTOU;
	pink_easy_call(ctx, _signal_immediately_func, &sig);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_CALLBACK_ABORT, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_CALLBACK_ABORT, errno, strerror(errno));

	pink_easy_context_destroy(ctx);
}
END_TEST

static short
_cb_exit(PINK_UNUSED const pink_easy_context_t *ctx, PINK_UNUSED pink_easy_process_t *proc, unsigned long status)
{
	if (WEXITSTATUS(status) != 127) {
		printf("%s: 127 != %i\n", __func__, WEXITSTATUS(status));
		return PINK_EASY_CALLBACK_ABORT;
	}
	return 0;
}

START_TEST(t_loop_exit)
{
	int ret;
	pink_easy_error_t e;
	pink_easy_callback_table_t tbl;
	pink_easy_context_t *ctx;

	memset(&tbl, 0, sizeof(pink_easy_callback_table_t));
	tbl.cb_event_exit = _cb_exit;
	ctx = pink_easy_context_new(PINK_TRACE_OPTION_SYSGOOD | PINK_TRACE_OPTION_EXIT, &tbl, NULL, NULL);
	fail_unless(ctx != NULL, "%d(%s)", errno, strerror(errno));

	ret = 127;
	pink_easy_call(ctx, _exit_immediately_func, &ret);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_SUCCESS, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_SUCCESS, errno, strerror(errno));

	ret = 128;
	pink_easy_call(ctx, _exit_immediately_func, &ret);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_CALLBACK_ABORT, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_CALLBACK_ABORT, errno, strerror(errno));

	pink_easy_context_destroy(ctx);
}
END_TEST

static bool _cb_exec_called = false;
static short
_cb_exec(PINK_UNUSED const pink_easy_context_t *ctx, PINK_UNUSED pink_easy_process_t *proc, PINK_UNUSED pink_bitness_t orig_bitness)
{
	_cb_exec_called = true;
	return 0;
}

static int
_exec_true_func(PINK_UNUSED void *data)
{
	char *const myargv[] = { "true", NULL };
	execvp("true", myargv);
	return 1;
}

START_TEST(t_loop_exec)
{
	pink_easy_error_t e;
	pink_easy_callback_table_t tbl;
	pink_easy_context_t *ctx;

	memset(&tbl, 0, sizeof(pink_easy_callback_table_t));
	tbl.cb_event_exec = _cb_exec;
	ctx = pink_easy_context_new(PINK_TRACE_OPTION_SYSGOOD | PINK_TRACE_OPTION_EXEC, &tbl, NULL, NULL);
	fail_unless(ctx != NULL, "%d(%s)", errno, strerror(errno));

	pink_easy_call(ctx, _exec_true_func, NULL);
	e = pink_easy_context_get_error(ctx);
	fail_unless(e == PINK_EASY_ERROR_SUCCESS, "%i != %i -> %d(%s)", e, PINK_EASY_ERROR_SUCCESS, errno, strerror(errno));
	fail_unless(_cb_exec_called, "wtf?");

	pink_easy_context_destroy(ctx);
}
END_TEST

START_TEST(t_loop_fork)
{
	/* TODO */
}
END_TEST

START_TEST(t_loop_vfork)
{
	/* TODO */
}
END_TEST

START_TEST(t_loop_clone)
{
	/* TODO */
}
END_TEST

Suite *
easy_loop_suite_create(void)
{
	Suite *s = suite_create("easy_loop");

	TCase *tc_pink_easy_loop = tcase_create("pink_easy_loop");

	tcase_add_test(tc_pink_easy_loop, t_loop_exit_genuine);
	tcase_add_test(tc_pink_easy_loop, t_loop_exit_signal);
	tcase_add_test(tc_pink_easy_loop, t_loop_genuine);
	tcase_add_test(tc_pink_easy_loop, t_loop_exit);
	tcase_add_test(tc_pink_easy_loop, t_loop_exec);
	tcase_add_test(tc_pink_easy_loop, t_loop_fork);

	tcase_add_test(tc_pink_easy_loop, t_loop_vfork);
	tcase_add_test(tc_pink_easy_loop, t_loop_clone);

	suite_add_tcase(s, tc_pink_easy_loop);

	return s;
}
