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
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "check_pinktrace.h"

#include <pinktrace/pink.h>

START_TEST(t_decode_stat)
{
	int status;
	long addr;
	pid_t pid;
	struct stat buf;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		stat("/dev/null", &buf);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the address of second argument */
		fail_unless(pink_util_get_arg(pid, PINKTRACE_DEFAULT_BITNESS, 1, &addr),
			"%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the exit of next system call */
		fail_unless(pink_trace_syscall_exit(pid, 0), "%d(%s)", errno, strerror(errno));
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Decode the stat structure */
		fail_unless(pink_util_move(pid, addr, &buf), "%d(%s)", errno, strerror(errno));
		fail_unless(S_ISCHR(buf.st_mode), "%#x", buf.st_mode);
		fail_unless(buf.st_rdev == 11, "11 != %d", buf.st_rdev);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_first)
{
	int status;
	pid_t pid;
	char buf[10];

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open("/dev/null", O_RDONLY);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_second)
{
	int status;
	pid_t pid;
	char buf[10];

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		openat(-1, "/dev/null", O_RDONLY);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_third)
{
	int status;
	pid_t pid;
	char buf[10];

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		symlinkat("/var/empty", AT_FDCWD, "/dev/null");
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_fourth)
{
	int status;
	pid_t pid;
	char buf[10];

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "/dev/null", 0600);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_null)
{
	int status;
	pid_t pid;
	char *buf;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open(NULL, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		buf = pink_decode_string_persistent(pid, PINKTRACE_DEFAULT_BITNESS, 0);
		fail_unless(buf == NULL, "NULL != `%s'", buf);
		fail_unless(errno == EIO || errno == EFAULT, "%d (%s)", errno, strerror(errno));

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_notrailingzero)
{
	int status;
	pid_t pid;
	char *buf;
	char notrailingzero[3] = {'n', 'i', 'l'};

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open(notrailingzero, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		buf = pink_decode_string_persistent(pid, PINKTRACE_DEFAULT_BITNESS, 0);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(buf[0] == 'n', "n != %c", buf[0]);
		fail_unless(buf[1] == 'i', "i != %c", buf[1]);
		fail_unless(buf[2] == 'l', "l != %c", buf[2]);

		free(buf);
		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_first)
{
	int status;
	pid_t pid;
	char *buf;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open("/dev/null", O_RDONLY);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		buf = pink_decode_string_persistent(pid, PINKTRACE_DEFAULT_BITNESS, 1);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_second)
{
	int status;
	pid_t pid;
	char *buf;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		openat(-1, "/dev/null", O_RDONLY);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		buf = pink_decode_string_persistent(pid, PINKTRACE_DEFAULT_BITNESS, 1);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_third)
{
	int status;
	pid_t pid;
	char *buf;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		symlinkat("/var/empty", AT_FDCWD, "/dev/null");
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		buf = pink_decode_string_persistent(pid, PINKTRACE_DEFAULT_BITNESS, 2);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_fourth)
{
	int status;
	pid_t pid;
	char *buf;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "/dev/null", 0600);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		buf = pink_decode_string_persistent(pid, PINKTRACE_DEFAULT_BITNESS, 3);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

Suite *
decode_suite_create(void)
{
	Suite *s = suite_create("decode");

	/* pink_decode_*() */
	TCase *tc_pink_decode = tcase_create("pink_decode");

	tcase_add_test(tc_pink_decode, t_decode_stat);

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
