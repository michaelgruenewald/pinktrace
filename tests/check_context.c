/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 *
 * This file is part of Pink's Tracing Library. pinktrace is free software; you
 * can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License version 2.1, as published by the Free Software
 * Foundation.
 *
 * pinktrace is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <errno.h>
#include <sys/types.h>
#include <string.h>

#include <check.h>

#include <pinktrace/gcc.h>
#include <pinktrace/context.h>
#include <pinktrace/error.h>
#include <pinktrace/step.h>
#include <pinktrace/trace.h>

#include "check_pinktrace.h"

static int
event_dummy(pink_unused const pink_context_t *ctx,
		pink_unused unsigned int event, pink_unused pid_t pid)
{
	return 0;
}

START_TEST(test_pink_context_new)
{
	pink_context_t *ctx;

	ctx = pink_context_new();

	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	pink_context_free(ctx);
}
END_TEST

START_TEST(test_pink_context_attach)
{
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	fail_unless(pink_context_get_attach(ctx) == false, "Attach property doesn't default to false");

	pink_context_set_attach(ctx, true);
	fail_unless(pink_context_get_attach(ctx) == true, "Failed to set attach property to true");

	pink_context_set_attach(ctx, false);
	fail_unless(pink_context_get_attach(ctx) == false, "Failed to set attach property to false");

	pink_context_free(ctx);
}
END_TEST

START_TEST(test_pink_context_options)
{
	int options;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	options = pink_context_get_options(ctx);
	fail_unless(options & PINK_TRACE_OPTION_SYSGOOD, "Options doesn't have SYSGOOD set by default");

	options &= ~PINK_TRACE_OPTION_SYSGOOD;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_unless(options & PINK_TRACE_OPTION_SYSGOOD, "Options doesn't force SYSGOOD");

	options |= PINK_TRACE_OPTION_FORK;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_unless(options & PINK_TRACE_OPTION_FORK, "Options doesn't have FORK set");

	options &= ~PINK_TRACE_OPTION_FORK;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_if(options & PINK_TRACE_OPTION_FORK, "Options have FORK set");

	options |= PINK_TRACE_OPTION_VFORK;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_unless(options & PINK_TRACE_OPTION_VFORK, "Options doesn't have VFORK set");

	options &= ~PINK_TRACE_OPTION_VFORK;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_if(options & PINK_TRACE_OPTION_VFORK, "Options have VFORK set");

	options |= PINK_TRACE_OPTION_CLONE;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_unless(options & PINK_TRACE_OPTION_CLONE, "Options doesn't have CLONE set");

	options &= ~PINK_TRACE_OPTION_CLONE;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_if(options & PINK_TRACE_OPTION_CLONE, "Options have CLONE set");

	options |= PINK_TRACE_OPTION_VFORKDONE;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_unless(options & PINK_TRACE_OPTION_VFORKDONE, "Options doesn't have VFORKDONE set");

	options &= ~PINK_TRACE_OPTION_VFORKDONE;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_if(options & PINK_TRACE_OPTION_VFORKDONE, "Options have VFORKDONE set");

	options |= PINK_TRACE_OPTION_EXIT;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_unless(options & PINK_TRACE_OPTION_EXIT, "Options doesn't have EXIT set");

	options &= ~PINK_TRACE_OPTION_EXIT;
	pink_context_set_options(ctx, options);
	options = pink_context_get_options(ctx);
	fail_if(options & PINK_TRACE_OPTION_EXIT, "Options have EXIT set");

	pink_context_free(ctx);
}
END_TEST

START_TEST(test_pink_context_eldest)
{
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	fail_unless(pink_context_get_eldest(ctx) < 0, "Wrong default for eldest process ID");

	pink_context_set_eldest(ctx, 123);
	fail_unless(pink_context_get_eldest(ctx) == 123, "Wrong process ID");

	pink_context_free(ctx);
}
END_TEST

START_TEST(test_pink_context_error)
{
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	fail_unless(pink_context_get_error(ctx) == PINK_ERROR_SUCCESS,
			"Wrong default for error, expected: %d got: %d",
			PINK_ERROR_SUCCESS, pink_context_get_error(ctx));

	/* TODO: Test more here (no modifier functions though ;) */
	pink_context_free(ctx);
}
END_TEST

START_TEST(test_pink_context_step)
{
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	fail_unless(pink_context_get_step(ctx) == PINK_STEP_SYSCALL,
			"Wrong default for step, expected: %d got: %d",
			PINK_STEP_SYSCALL, pink_context_get_step(ctx));

	pink_context_set_step(ctx, PINK_STEP_SINGLE);
	fail_unless(pink_context_get_step(ctx) == PINK_STEP_SINGLE,
			"Failed to set step, expected: %d got: %d",
			PINK_STEP_SINGLE, pink_context_get_step(ctx));

	pink_context_free(ctx);
}
END_TEST

START_TEST(test_pink_context_handler)
{
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	fail_unless(pink_context_get_handler(ctx) == NULL,
			"Wrong default for the event handler");

	pink_context_set_handler(ctx, (pink_event_func_t)event_dummy);
	fail_if(pink_context_get_handler(ctx) == NULL,
			"Failed to set event handler");

	pink_context_free(ctx);
}
END_TEST

Suite *
context_suite_create(void)
{
	Suite *s = suite_create("context");

	/* pink_context_new() */
	TCase *tc_pink_context_new = tcase_create("pink_context_new");

	tcase_add_test(tc_pink_context_new, test_pink_context_new);

	suite_add_tcase(s, tc_pink_context_new);

	/* pink_context_{g,s}_* */
	TCase *tc_pink_context_properties = tcase_create("pink_context_properties");

	tcase_add_test(tc_pink_context_properties, test_pink_context_attach);
	tcase_add_test(tc_pink_context_properties, test_pink_context_options);
	tcase_add_test(tc_pink_context_properties, test_pink_context_eldest);
	tcase_add_test(tc_pink_context_properties, test_pink_context_error);
	tcase_add_test(tc_pink_context_properties, test_pink_context_step);
	tcase_add_test(tc_pink_context_properties, test_pink_context_handler);

	suite_add_tcase(s, tc_pink_context_properties);

	return s;
}
