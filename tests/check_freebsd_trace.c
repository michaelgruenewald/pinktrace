/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

#include "check_pinktrace.h"

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
		/* FIXME: Why do we need to attach to the process here? */
		if (!pink_trace_attach(pid))
			fail("pink_trace_attach: %d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_resume(pid, 0), "%d(%s)", errno, strerror(errno));

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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_resume(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_kill(pid), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_singlestep(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_singlestep(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_entry(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_exit(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_syscall_exit(pid, SIGTERM), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
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

	/* pink_trace_cont() & pink_trace_resume() */
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
