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
#include <string.h>

#include <check.h>

#include <pinktrace/context.h>
#include <pinktrace/trace.h>

#include "check_pinktrace.h"

START_TEST(test_pink_context_new)
{
	pink_context_t *ctx;

	ctx = pink_context_new();

	fail_unless(ctx, "pink_context_new failed: %s", strerror(errno));

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

	return s;
}
