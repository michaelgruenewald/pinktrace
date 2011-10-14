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
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "check_pinktrace.h"

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

		fail_unless(pink_util_get_syscall(pid, PINKTRACE_BITNESS_DEFAULT, &scno),
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

		fail_unless(pink_util_set_syscall(pid, PINKTRACE_BITNESS_DEFAULT, PINKTRACE_INVALID_SYSCALL),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_syscall(pid, PINKTRACE_BITNESS_DEFAULT, &scno),
			"%d(%s)", errno, strerror(errno));
		fail_unless(scno == PINKTRACE_INVALID_SYSCALL, "%ld != %ld", PINKTRACE_INVALID_SYSCALL, scno);

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

		fail_unless(pink_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
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

		fail_unless(pink_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
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

		fail_unless(pink_util_set_return(pid, pid + 1), "%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
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

		fail_unless(pink_util_set_return(pid, -ENAMETOOLONG), "%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_return(pid, &ret), "%d(%s)", errno, strerror(errno));
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

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 0, &arg),
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

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 1, &arg),
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

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 2, &arg),
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

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 3, &arg),
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

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 4, &arg),
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

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 5, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

/* XXX */
START_TEST(t_util_set_arg_first)
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
		mmap((void *)0, 0, 0, 0, 0, 0);
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

		fail_unless(pink_util_set_arg(pid, PINKTRACE_BITNESS_DEFAULT, 0, 13),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 0, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_arg_second)
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
		mmap(NULL, 0, 0, 0, 0, 0);
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

		fail_unless(pink_util_set_arg(pid, PINKTRACE_BITNESS_DEFAULT, 1, 13),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 1, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_arg_third)
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
		mmap(NULL, 0, 0, 0, 0, 0);
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

		fail_unless(pink_util_set_arg(pid, PINKTRACE_BITNESS_DEFAULT, 2, 13),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 2, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_arg_fourth)
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
		mmap(NULL, 0, 0, 0, 0, 0);
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

		fail_unless(pink_util_set_arg(pid, PINKTRACE_BITNESS_DEFAULT, 3, 13),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 3, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_arg_fifth)
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
		mmap(NULL, 0, 0, 0, 0, 0);
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

		fail_unless(pink_util_set_arg(pid, PINKTRACE_BITNESS_DEFAULT, 4, 13),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 4, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "13 != %ld", arg);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_util_set_arg_sixth)
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
		mmap(NULL, 0, 0, 0, 0, 0);
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

		fail_unless(pink_util_set_arg(pid, PINKTRACE_BITNESS_DEFAULT, 5, 13),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 5, &arg),
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

	/* pink_util_* */
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
	tcase_add_test(tc_pink_util, t_util_set_arg_first);
	tcase_add_test(tc_pink_util, t_util_set_arg_second);
	tcase_add_test(tc_pink_util, t_util_set_arg_third);
	tcase_add_test(tc_pink_util, t_util_set_arg_fourth);
	tcase_add_test(tc_pink_util, t_util_set_arg_fifth);
	tcase_add_test(tc_pink_util, t_util_set_arg_sixth);

	suite_add_tcase(s, tc_pink_util);

	return s;
}
