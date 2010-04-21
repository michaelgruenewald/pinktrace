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
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace/pink.h>

#include "check_pinktrace.h"

START_TEST(test_pink_util_get_syscall)
{
	int status;
	long scno;
	pid_t pid;
	pink_event_t event;
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
	else if (!pid) { /* child */
		/* From getpid(2):
		 * Since glibc version 2.3.4, the glibc wrapper function for getpid()
		 * caches PIDs, ...
		 */
		/* Since the child has just called getpid() to send herself a SIGSTOP,
		 * calling it again won't call the system call hence we need to use
		 * syscall(2) here.
		 */
		syscall(SYS_getpid);
	}
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0),
				"pink_trace_syscall failed: %s",
				strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(ctx, status);
		fail_unless(event == PINK_EVENT_SYSCALL,
				"Wrong event, expected: %d got: %d",
				PINK_EVENT_SYSCALL, event);

		fail_unless(pink_util_get_syscall(pid, &scno),
				"pink_util_get_syscall failed: %s",
				strerror(errno));
		fail_unless(scno == SYS_getpid,
				"Wrong syscall, expected: %ld got: %ld",
				SYS_getpid, scno);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(test_pink_util_set_syscall)
{
	int status;
	long scno;
	pid_t pid;
	pink_event_t event;
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
	else if (!pid) { /* child */
		/* From getpid(2):
		 * Since glibc version 2.3.4, the glibc wrapper function for getpid()
		 * caches PIDs, ...
		 */
		/* Since the child has just called getpid() to send herself a SIGSTOP,
		 * calling it again won't call the system call hence we need to use
		 * syscall(2) here.
		 */
		syscall(SYS_getpid);
	}
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0),
				"pink_trace_syscall failed: %s",
				strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(ctx, status);
		fail_unless(event == PINK_EVENT_SYSCALL,
				"Wrong event, expected: %d got: %d",
				PINK_EVENT_SYSCALL, event);

		fail_unless(pink_util_set_syscall(pid, 0xbadca11),
				"pink_util_set_syscall failed: %s",
				strerror(errno));
		fail_unless(pink_util_get_syscall(pid, &scno),
				"pink_util_get_syscall failed: %s",
				strerror(errno));
		fail_unless(scno == 0xbadca11,
				"Wrong syscall, expected: %ld got: %ld",
				0xbadca11, scno);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST


Suite *
util_suite_create(void)
{
	Suite *s = suite_create("util");

	/* pink_util_*() */
	TCase *tc_pink_util = tcase_create("pink_util");

	tcase_add_test(tc_pink_util, test_pink_util_get_syscall);
	tcase_add_test(tc_pink_util, test_pink_util_set_syscall);

	suite_add_tcase(s, tc_pink_util);

	return s;
}
