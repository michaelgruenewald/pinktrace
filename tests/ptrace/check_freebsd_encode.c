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

#include "check_pinktrace_ptrace.h"

#include <errno.h>
#include <sys/stat.h>
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

START_TEST(t_encode_string_first_lensame)
{
	int status;
	char buf[10];
	pid_t pid;

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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 0, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_first_lenshort)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open("pi", O_RDONLY);
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 0, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_first_lenlong)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open("3,14159265", O_RDONLY);
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 0, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_second_lensame)
{
	int status;
	char buf[10];
	pid_t pid;

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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_second_lenshort)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		openat(-1, "pi", O_RDONLY);
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_second_lenlong)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		openat(-1, "3,14159265", O_RDONLY);
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_third_lensame)
{
	int status;
	char buf[10];
	pid_t pid;

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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 2, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_third_lenshort)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		symlinkat("/var/empty", AT_FDCWD, "pi");
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 2, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_third_lenlong)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		symlinkat("/var/empty", AT_FDCWD, "3,14159265");
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 2, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_fourth_lensame)
{
	int status;
	char buf[10];
	pid_t pid;

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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 3, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_fourth_lenshort)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "pi", 0600);
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 3, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_fourth_lenlong)
{
	int status;
	char buf[10];
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "3,14159265", 0600);
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

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 3, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_stat)
{
	int status;
	struct stat buf, newbuf;
	pid_t pid;

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

		/* Fill in the stat structure */
		memset(&buf, 0, sizeof(struct stat));
		buf.st_mode = S_IFCHR;
		buf.st_rdev = 11; /* /dev/null */

		fail_unless(pink_trace_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, &buf, sizeof(struct stat)),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_decode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, &newbuf, sizeof(struct stat)),
			"%d(%s)", errno, strerror(errno));
		fail_unless(S_ISCHR(newbuf.st_mode), "%d", newbuf.st_mode);
		fail_unless(newbuf.st_rdev == 11, "%d != %d", 11, newbuf.st_rdev);

		pink_trace_kill(pid);
	}
}
END_TEST

Suite *
encode_suite_create(void)
{
	Suite *s = suite_create("encode");

	/* pink_trace_encode_*() */
	TCase *tc_pink_trace_encode = tcase_create("pink_trace_encode");

	tcase_add_test(tc_pink_trace_encode, t_encode_string_first_lensame);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_first_lenshort);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_first_lenlong);

	tcase_add_test(tc_pink_trace_encode, t_encode_string_second_lensame);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_second_lenshort);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_second_lenlong);

	tcase_add_test(tc_pink_trace_encode, t_encode_string_third_lensame);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_third_lenshort);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_third_lenlong);

	tcase_add_test(tc_pink_trace_encode, t_encode_string_fourth_lensame);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_fourth_lenshort);
	tcase_add_test(tc_pink_trace_encode, t_encode_string_fourth_lenlong);

	tcase_add_test(tc_pink_trace_encode, t_encode_stat);

	suite_add_tcase(s, tc_pink_trace_encode);

	return s;
}
