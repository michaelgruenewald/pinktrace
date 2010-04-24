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

#ifndef _ATFILE_SOURCE
#define _ATFILE_SOURCE 1
#endif /* !_ATFILE_SOURCE */

#include <errno.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace/pink.h>

#include "check_pinktrace.h"

/* Check bitness */
#if defined(I386) || defined(POWERPC)
#define CHECK_BITNESS (PINK_BITNESS_32)
#elif defined(X86_64) || defined(IA64) || defined(POWERPC64)
#define CHECK_BITNESS (PINK_BITNESS_64)
#else
#error unsupported architecture
#endif

START_TEST(t_util_get_syscall)
{
	int status;
	long scno;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
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

START_TEST(t_util_set_syscall)
{
	int status;
	long scno;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
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

START_TEST(t_util_get_return_success)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
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

		/* Resume the child and it will stop at the end of next system call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall failed: %s",
					strerror(errno));

			/* Make sure we got the right event */
			waitpid(pid, &status, 0);
			event = pink_event_decide(ctx, status);
			fail_unless(event == PINK_EVENT_SYSCALL,
					"Wrong event, expected: %d got: %d",
					PINK_EVENT_SYSCALL, event);
		}

		fail_unless(pink_util_get_return(pid, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == pid,
				"Wrong return, expected: %i got: %d",
				pid, ret);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_util_get_return_fail)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		open(NULL, 0); /* Should fail with -EFAULT */
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);

		/* Resume the child and it will stop at the end of next system call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall failed: %s",
					strerror(errno));

			/* Make sure we got the right event */
			waitpid(pid, &status, 0);
			event = pink_event_decide(ctx, status);
			fail_unless(event == PINK_EVENT_SYSCALL,
					"Wrong event, expected: %d got: %d",
					PINK_EVENT_SYSCALL, event);
		}

		fail_unless(pink_util_get_return(pid, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == -EFAULT,
				"Wrong return, expected: %ld got: %ld",
				-EFAULT, ret);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_util_set_return_success)
{
	int ret, status;
	pid_t pid, mypid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) { /* child */
		/* From getpid(2):
		 * Since glibc version 2.3.4, the glibc wrapper function for getpid()
		 * caches PIDs, ...
		 */
		/* Since the child has just called getpid() to send herself a SIGSTOP,
		 * calling it again won't call the system call hence we need to use
		 * syscall(2) here.
		 */
		mypid = getpid();
		ret = syscall(SYS_getpid);
		if (ret != (mypid + 1)) {
			fprintf(stderr, "Wrong return, expected: %i got: %i\n",
					mypid + 1, ret);
			_exit(EXIT_FAILURE);
		}
		_exit(EXIT_SUCCESS);
	}
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);

		/* Resume the child and it will stop at the end of next system call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall failed: %s",
					strerror(errno));

			/* Make sure we got the right event */
			waitpid(pid, &status, 0);
			event = pink_event_decide(ctx, status);
			fail_unless(event == PINK_EVENT_SYSCALL,
					"Wrong event, expected: %d got: %d",
					PINK_EVENT_SYSCALL, event);
		}

		fail_unless(pink_util_set_return(pid, pid + 1),
				"pink_util_set_return failed: %s",
				strerror(errno));

		/* Let the child exit and check her exit status */
		fail_unless(pink_trace_cont(pid, 0),
				"pink_trace_cont failed: %s",
				strerror(errno));
		waitpid(pid, &status, 0);
		fail_unless(WEXITSTATUS(status) == EXIT_SUCCESS,
				"Child returned non-zero");

		pink_context_free(ctx);
	}
}
END_TEST

START_TEST(t_util_set_return_fail)
{
	int ret, status;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) { /* child */
		/* From getpid(2):
		 * Since glibc version 2.3.4, the glibc wrapper function for getpid()
		 * caches PIDs, ...
		 */
		/* Since the child has just called getpid() to send herself a SIGSTOP,
		 * calling it again won't call the system call hence we need to use
		 * syscall(2) here.
		 */
		ret = syscall(SYS_getpid);
		if (ret > 0) {
			fprintf(stderr, "ret: %i\n", ret);
			_exit(EXIT_FAILURE);
		}
		else if (errno != ENAMETOOLONG) {
			fprintf(stderr, "errno: %d (%s)\n",
					errno, strerror(errno));
			_exit(EXIT_FAILURE);
		}
		_exit(EXIT_SUCCESS);
	}
	else { /* parent */
		fail_unless(pink_context_get_eldest(ctx) == pid,
				"Wrong eldest pid, expected: %d got: %d",
				pink_context_get_eldest(ctx), pid);

		/* Resume the child and it will stop at the end of next system call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall failed: %s",
					strerror(errno));

			/* Make sure we got the right event */
			waitpid(pid, &status, 0);
			event = pink_event_decide(ctx, status);
			fail_unless(event == PINK_EVENT_SYSCALL,
					"Wrong event, expected: %d got: %d",
					PINK_EVENT_SYSCALL, event);
		}

		fail_unless(pink_util_set_return(pid, -ENAMETOOLONG),
				"pink_util_set_return failed: %s",
				strerror(errno));

		/* Let the child exit and check her exit status */
		fail_unless(pink_trace_cont(pid, 0),
				"pink_trace_cont failed: %s",
				strerror(errno));
		waitpid(pid, &status, 0);
		fail_unless(WEXITSTATUS(status) == EXIT_SUCCESS,
				"Child returned non-zero");

		pink_context_free(ctx);
	}
}
END_TEST

START_TEST(t_util_get_arg_first)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		mmap((void *)13, 0, 0, 0, 0, 0);
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

		fail_unless(pink_util_get_arg(pid, CHECK_BITNESS, 0, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == 13,
				"Wrong return, expected: %ld got: %ld",
				13, ret);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_util_get_arg_second)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		mmap((void *)0, 13, 0, 0, 0, 0);
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

		fail_unless(pink_util_get_arg(pid, CHECK_BITNESS, 1, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == 13,
				"Wrong return, expected: %ld got: %ld",
				13, ret);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_util_get_arg_third)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		mmap((void *)0, 0, 13, 0, 0, 0);
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

		fail_unless(pink_util_get_arg(pid, CHECK_BITNESS, 2, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == 13,
				"Wrong return, expected: %ld got: %ld",
				13, ret);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_util_get_arg_fourth)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		mmap((void *)0, 0, 0, 13, 0, 0);
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

		fail_unless(pink_util_get_arg(pid, CHECK_BITNESS, 3, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == 13,
				"Wrong return, expected: %ld got: %ld",
				13, ret);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_util_get_arg_fifth)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		mmap((void *)0, 0, 0, 0, 13, 0);
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

		fail_unless(pink_util_get_arg(pid, CHECK_BITNESS, 4, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == 13,
				"Wrong return, expected: %ld got: %ld",
				13, ret);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_util_get_arg_sixth)
{
	int status;
	long ret;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		mmap((void *)0, 0, 0, 0, 0, 13);
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

		fail_unless(pink_util_get_arg(pid, CHECK_BITNESS, 5, &ret),
				"pink_util_get_return failed: %s",
				strerror(errno));
		fail_unless(ret == 13,
				"Wrong return, expected: %ld got: %ld",
				13, ret);

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

	tcase_add_test(tc_pink_util, t_util_get_syscall);
	tcase_add_test(tc_pink_util, t_util_set_syscall);
	tcase_add_test(tc_pink_util, t_util_get_return_success);
	tcase_add_test(tc_pink_util, t_util_get_return_fail);
	tcase_add_test(tc_pink_util, t_util_set_return_success);
	tcase_add_test(tc_pink_util, t_util_set_return_fail);
	tcase_add_test(tc_pink_util, t_util_get_arg_first);
	tcase_add_test(tc_pink_util, t_util_get_arg_second);
	tcase_add_test(tc_pink_util, t_util_get_arg_third);
	tcase_add_test(tc_pink_util, t_util_get_arg_fourth);
	tcase_add_test(tc_pink_util, t_util_get_arg_fifth);
	tcase_add_test(tc_pink_util, t_util_get_arg_sixth);

	suite_add_tcase(s, tc_pink_util);

	return s;
}
