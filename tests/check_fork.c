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
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace/context.h>
#include <pinktrace/fork.h>

#include "check_pinktrace.h"

START_TEST(test_pink_fork)
{
	pid_t pid;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0) {
		switch (pid) {
		case PINK_FORK_ERROR_FORK:
			fail("fork failed: %s", strerror(errno));
		case PINK_FORK_ERROR_TRACE:
			fail("pink_trace_me failed: %s", strerror(errno));
		case PINK_FORK_ERROR_SETUP:
			fail("pink_trace_setup failed: %s", strerror(errno));
		default:
			fail("unknown return code by pink_fork %d", pid);
		}
	}
	else if (!pid) /* child */
		kill(getpid(), SIGSTOP);
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);
		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

#if 0
#error This test segfaults for some reason
START_TEST(test_pink_vfork)
{
	pid_t pid;
	pink_context_t *ctx;
	char *const myargv[] = { "/bin/true", NULL};

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_vfork(ctx)) < 0) {
		switch (pid) {
		case PINK_FORK_ERROR_FORK:
			fail("vfork failed: %s", strerror(errno));
		case PINK_FORK_ERROR_TRACE:
			fail("pink_trace_me failed: %s", strerror(errno));
		case PINK_FORK_ERROR_SETUP:
			fail("pink_trace_setup failed: %s", strerror(errno));
		default:
			fail("unknown return code by pink_vfork %d", pid);
		}
	}
	else if (!pid) /* child */
		execvp("/bin/true", myargv);
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);
		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST
#endif

Suite *
fork_suite_create(void)
{
	Suite *s = suite_create("fork");

	/* pink_fork() */
	TCase *tc_pink_fork = tcase_create("pink_fork");

	tcase_add_test(tc_pink_fork, test_pink_fork);

	suite_add_tcase(s, tc_pink_fork);

#if 0
	/* pink_vfork() */
	TCase *tc_pink_vfork = tcase_create("pink_vfork");

	tcase_add_test(tc_pink_vfork, test_pink_vfork);

	suite_add_tcase(s, tc_pink_vfork);
#endif

	return s;
}
