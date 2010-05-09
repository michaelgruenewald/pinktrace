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
#include <unistd.h>

#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "check_pinktrace_ptrace.h"

START_TEST(t_util_get_syscall)
{
	int status;
	long scno;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_trace_util_get_syscall(pid, PINKTRACE_DEFAULT_BITNESS, &scno),
			"%d(%s)", errno, strerror(errno));
		fail_unless(scno == SYS_getpid, "%ld != %ld", SYS_getpid, scno);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_syscall)
{
	int status;
	long scno;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_trace_util_set_syscall(pid, PINKTRACE_DEFAULT_BITNESS, 0xbadca11),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_util_get_syscall(pid, PINKTRACE_DEFAULT_BITNESS, &scno),
			"%d(%s)", errno, strerror(errno));
		fail_unless(scno == 0xbadca11, "%ld != %ld", 0xbadca11, scno);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_return_success)
{
	int status;
	long ret;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the end of next system call */
		fail_unless(pink_trace_syscall_exit(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_trace_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
		fail_unless(ret == pid, "%ld != %ld", pid, ret);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_return_fail)
{
	int status;
	long ret;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open(NULL, 0); /* Should fail with EFAULT */
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the end of next system call */
		fail_unless(pink_trace_syscall_exit(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_trace_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
		fail_unless(ret == -EFAULT, "%ld != %ld", -EFAULT, ret);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_return_success)
{
	int status;
	long ret;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the end of next system call */
		fail_unless(pink_trace_syscall_exit(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_trace_util_set_return(pid, pid + 1), "%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
		fail_unless(ret == pid + 1, "%ld != %ld", pid + 1, ret);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_return_fail)
{
	int status;
	long ret;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the end of next system call */
		fail_unless(pink_trace_syscall_exit(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		fail_unless(pink_trace_util_set_return(pid, -ENAMETOOLONG), "%d(%s)", errno, strerror(errno));
		fail_unless(pink_trace_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
		fail_unless(ret == -ENAMETOOLONG, "%ld != %ld", -ENAMETOOLONG, ret);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_arg_first)
{
	int status;
	long arg;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		mmap((void *)13, 0, 0, 0, 0, 0);
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

		fail_unless(pink_trace_util_get_arg(pid, PINKTRACE_DEFAULT_BITNESS, 0, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_arg_second)
{
	int status;
	long arg;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		mmap(NULL, 13, 0, 0, 0, 0);
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

		fail_unless(pink_trace_util_get_arg(pid, PINKTRACE_DEFAULT_BITNESS, 1, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_arg_third)
{
	int status;
	long arg;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 13, 0, 0, 0);
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

		fail_unless(pink_trace_util_get_arg(pid, PINKTRACE_DEFAULT_BITNESS, 2, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_arg_fourth)
{
	int status;
	long arg;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 0, 13, 0, 0);
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

		fail_unless(pink_trace_util_get_arg(pid, PINKTRACE_DEFAULT_BITNESS, 3, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_arg_fifth)
{
	int status;
	long arg;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 0, 0, 13, 0);
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

		fail_unless(pink_trace_util_get_arg(pid, PINKTRACE_DEFAULT_BITNESS, 4, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_get_arg_sixth)
{
	int status;
	long arg;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 0, 0, 0, 13);
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

		fail_unless(pink_trace_util_get_arg(pid, PINKTRACE_DEFAULT_BITNESS, 5, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

Suite *
util_suite_create(void)
{
	Suite *s = suite_create("util");

	/* pink_trace_util_* */
	TCase *tc_pink_trace_util = tcase_create("pink_trace_util");

	tcase_add_test(tc_pink_trace_util, t_util_get_syscall);
	tcase_add_test(tc_pink_trace_util, t_util_set_syscall);
	tcase_add_test(tc_pink_trace_util, t_util_get_return_success);
	tcase_add_test(tc_pink_trace_util, t_util_get_return_fail);
	tcase_add_test(tc_pink_trace_util, t_util_set_return_success);
	tcase_add_test(tc_pink_trace_util, t_util_set_return_fail);
	tcase_add_test(tc_pink_trace_util, t_util_get_arg_first);
	tcase_add_test(tc_pink_trace_util, t_util_get_arg_second);
	tcase_add_test(tc_pink_trace_util, t_util_get_arg_third);
	tcase_add_test(tc_pink_trace_util, t_util_get_arg_fourth);
	tcase_add_test(tc_pink_trace_util, t_util_get_arg_fifth);
	tcase_add_test(tc_pink_trace_util, t_util_get_arg_sixth);

	suite_add_tcase(s, tc_pink_trace_util);

	return s;
}
