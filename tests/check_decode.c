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

START_TEST(t_decode_string_first)
{
	int status;
	char buf[10];
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		open("/dev/null", O_RDONLY);
	else { /* parent */
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

		fail_unless(pink_decode_string(pid, CHECK_BITNESS, 0, buf, 10),
				"pink_decode_string: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_second)
{
	int status;
	char buf[10];
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		openat(-1, "/dev/null", O_RDONLY);
	else { /* parent */
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

		fail_unless(pink_decode_string(pid, CHECK_BITNESS, 1, buf, 10),
				"pink_decode_string: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_third)
{
	int status;
	char buf[10];
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		symlinkat("/var/empty", AT_FDCWD, "/dev/null");
	else { /* parent */
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

		fail_unless(pink_decode_string(pid, CHECK_BITNESS, 2, buf, 10),
				"pink_decode_string: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_fourth)
{
	int status;
	char buf[10];
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "/dev/null", 0600);
	else { /* parent */
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

		fail_unless(pink_decode_string(pid, CHECK_BITNESS, 3, buf, 10),
				"pink_decode_string: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_null)
{
	int status;
	char *buf;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		open(NULL, O_RDONLY);
	else { /* parent */
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

		buf = pink_decode_string_persistent(pid, CHECK_BITNESS, 0);
		fail_if(buf != NULL, "Wrong string, expected: NULL got: %s", buf);
		fail_unless(errno == EIO || errno == EFAULT,
				"Wrong errno, expected: EIO/EFAULT got: %d (%s)",
				errno, strerror(errno));

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_notrailingzero)
{
	int status;
	char *buf;
	char notrailingzero[3] = {'n', 'i', 'l'};
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		open(notrailingzero, O_RDONLY);
	else { /* parent */
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

		buf = pink_decode_string_persistent(pid, CHECK_BITNESS, 0);
		fail_if(buf == NULL,
				"pink_decode_string_persistent failed: %s",
				strerror(errno));
		fail_unless(buf[0] == 'n',
				"Wrong first char, expected: %c got: %c",
				'n', buf[0]);
		fail_unless(buf[1] == 'i',
				"Wrong second char, expected: %c got: %c",
				'i', buf[1]);
		fail_unless(buf[2] == 'l',
				"Wrong third char, expected: %c got: %c",
				'l', buf[2]);

		free(buf);
		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_first)
{
	int status;
	char *buf;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		open("/dev/null", O_RDONLY);
	else { /* parent */
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

		buf = pink_decode_string_persistent(pid, CHECK_BITNESS, 0);
		fail_if(buf == NULL,
				"pink_decode_string_persistent: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		free(buf);
		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_second)
{
	int status;
	char *buf;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		openat(-1, "/dev/null", O_RDONLY);
	else { /* parent */
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

		buf = pink_decode_string_persistent(pid, CHECK_BITNESS, 1);
		fail_if(buf == NULL,
				"pink_decode_string_persistent: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		free(buf);
		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_third)
{
	int status;
	char *buf;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		symlinkat("/var/empty", AT_FDCWD, "/dev/null");
	else { /* parent */
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

		buf = pink_decode_string_persistent(pid, CHECK_BITNESS, 2);
		fail_if(buf == NULL,
				"pink_decode_string_persistent: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		free(buf);
		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_fourth)
{
	int status;
	char *buf;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "/dev/null", 0600);
	else { /* parent */
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

		buf = pink_decode_string_persistent(pid, CHECK_BITNESS, 3);
		fail_if(buf == NULL,
				"pink_decode_string_persistent: %s",
				strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10),
				"Wrong string: expected /dev/null got `%s'",
				buf);

		free(buf);
		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

Suite *
decode_suite_create(void)
{
	Suite *s = suite_create("decode");

	/* pink_util_*() */
	TCase *tc_pink_decode = tcase_create("pink_decode");

	tcase_add_test(tc_pink_decode, t_decode_string_first);
	tcase_add_test(tc_pink_decode, t_decode_string_second);
	tcase_add_test(tc_pink_decode, t_decode_string_third);
	tcase_add_test(tc_pink_decode, t_decode_string_fourth);
	tcase_add_test(tc_pink_decode, t_decode_string_persistent_null);
	tcase_add_test(tc_pink_decode, t_decode_string_persistent_notrailingzero);
	tcase_add_test(tc_pink_decode, t_decode_string_persistent_first);
	tcase_add_test(tc_pink_decode, t_decode_string_persistent_second);
	tcase_add_test(tc_pink_decode, t_decode_string_persistent_third);
	tcase_add_test(tc_pink_decode, t_decode_string_persistent_fourth);

	suite_add_tcase(s, tc_pink_decode);

	return s;
}
