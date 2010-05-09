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
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace/trace.h>

#include "check_pinktrace_ptrace.h"

START_TEST(t_trace_me_basic)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		_exit(EXIT_SUCCESS);
	}
	else { /* parent */
		waitpid(pid, &status, 0);
		fail_unless(WEXITSTATUS(status) == EXIT_SUCCESS, "%#x", status);
	}
}
END_TEST

START_TEST(t_trace_me_signal)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGTTIN);
		_exit(EXIT_FAILURE);
	}
	else { /* parent */
		waitpid(pid, &status, 0);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_trace_me_execve)
{
	int status;
	pid_t pid;
	char *const myargv[] = { "/bin/true", NULL };

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		execvp("/bin/true", myargv);
		_exit(EXIT_FAILURE);
	}
	else { /* parent */
		if (!pink_trace_attach(pid))
			fail("pink_attach: %d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(t_trace_cont_basic)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else {
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_cont(pid, 0, (char *)1), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFEXITED(status), "%#x", status);
		fail_unless(WEXITSTATUS(status) == 13, "%#x", status);
	}
}
END_TEST


START_TEST(t_trace_cont_signal)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_cont(pid, SIGTERM, (char *)1), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSIGNALED(status), "%#x", status);
		fail_unless(WTERMSIG(status) == SIGTERM, "%#x", status);
	}
}
END_TEST

START_TEST(t_trace_kill)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_kill(pid), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSIGNALED(status), "%#x", status);
		fail_unless(WTERMSIG(status) == SIGKILL, "%#x", status);
	}
}
END_TEST

START_TEST(t_trace_singlestep_basic)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_singlestep(pid, 0), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_trace_singlestep_signal)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_singlestep(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSIGNALED(status), "%#x", status);
		fail_unless(WTERMSIG(status) == SIGTERM, "%#x", status);
	}
}
END_TEST

START_TEST(t_trace_syscall_basic)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_trace_syscall_signal)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSIGNALED(status), "%#x", status);
		fail_unless(WTERMSIG(status) == SIGTERM, "%#x", status);
	}
}
END_TEST

START_TEST(t_trace_syscall_entry_basic)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_trace_syscall_entry_signal)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_entry(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSIGNALED(status), "%#x", status);
		fail_unless(WTERMSIG(status) == SIGTERM, "%#x", status);
	}
}
END_TEST

START_TEST(t_trace_syscall_exit_basic)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		kill(getpid(), 0);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_exit(pid, 0), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_trace_syscall_exit_signal)
{
	int status;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		kill(getpid(), SIGSTOP);
		_exit(13);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_exit(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		waitpid(pid, &status, 0);
		fail_unless(WIFSIGNALED(status), "%#x", status);
		fail_unless(WTERMSIG(status) == SIGTERM, "%#x", status);
	}
}
END_TEST

Suite *
trace_suite_create(void)
{
	Suite *s = suite_create("trace");

	TCase *tc_pink_trace = tcase_create("pink_trace");

	/* pink_trace_me() */
	tcase_add_test(tc_pink_trace, t_trace_me_basic);
	tcase_add_test(tc_pink_trace, t_trace_me_signal);
	tcase_add_test(tc_pink_trace, t_trace_me_execve);

	/* pink_trace_cont() */
	tcase_add_test(tc_pink_trace, t_trace_cont_basic);
	tcase_add_test(tc_pink_trace, t_trace_cont_signal);

	/* pink_trace_kill() */
	tcase_add_test(tc_pink_trace, t_trace_kill);

	/* pink_trace_singlestep() */
	tcase_add_test(tc_pink_trace, t_trace_singlestep_basic);
	tcase_add_test(tc_pink_trace, t_trace_singlestep_signal);

	/* pink_trace_syscall*() */
	tcase_add_test(tc_pink_trace, t_trace_syscall_basic);
	tcase_add_test(tc_pink_trace, t_trace_syscall_signal);
	tcase_add_test(tc_pink_trace, t_trace_syscall_entry_basic);
	tcase_add_test(tc_pink_trace, t_trace_syscall_entry_signal);
	tcase_add_test(tc_pink_trace, t_trace_syscall_exit_basic);
	tcase_add_test(tc_pink_trace, t_trace_syscall_exit_signal);

	suite_add_tcase(s, tc_pink_trace);

	return s;
}
