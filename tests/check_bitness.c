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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <errno.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace/bitness.h>
#include <pinktrace/context.h>
#include <pinktrace/error.h>
#include <pinktrace/fork.h>

#include "check_pinktrace.h"

/* Check bitness */
#if defined(I386) || defined(POWERPC)
#define CHECK_BITNESS (PINK_BITNESS_32)
#elif defined(X86_64) || defined(IA64) || defined(POWERPC64)
#define CHECK_BITNESS (PINK_BITNESS_64)
#else
#error unsupported architecture
#endif

START_TEST(test_pink_bitness)
{
	pid_t pid;
	pink_bitness_t bitness;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0) {
		switch (pink_context_get_error(ctx)) {
		case PINK_ERROR_FORK:
			fail("fork failed: %s", strerror(errno));
		case PINK_ERROR_TRACE:
			fail("pink_trace_me failed: %s", strerror(errno));
		case PINK_ERROR_TRACE_SETUP:
			fail("pink_trace_setup failed: %s", strerror(errno));
		default:
			fail("unknown return code by pink_fork %d", pid);
		}
	}
	else if (!pid) /* child */
		pause();
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);

		bitness = pink_bitness_get(pid);
		fail_unless(bitness == CHECK_BITNESS,
				"Wrong bitness, expected: %d got: %d",
				CHECK_BITNESS, bitness);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

Suite *
bitness_suite_create(void)
{
	Suite *s = suite_create("bitness");

	/* pink_bitness_get() */
	TCase *tc_pink_bitness = tcase_create("pink_bitness");

	tcase_add_test(tc_pink_bitness, test_pink_bitness);

	suite_add_tcase(s, tc_pink_bitness);

	return s;
}
