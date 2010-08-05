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

#include "check_pinktrace.h"

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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 0, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 0, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 0, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 2, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 2, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 2, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 3, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 3, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
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
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 3, "/dev/zero", 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_string(pid, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/zero", 10) == 0, "/dev/zero != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_encode_string_safe_first_lensame)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_first_lenshort)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_first_lenlong)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_second_lensame)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_second_lenshort)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_second_lenlong)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_third_lensame)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_third_lenshort)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_third_lenlong)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_fourth_lensame)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_fourth_lenshort)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_string_safe_fourth_lenlong)
{
	/* TODO: Write a smart test case */
}
END_TEST

START_TEST(t_encode_stat)
{
	int status;
	struct stat buf, newbuf;
	pid_t pid;
	pink_event_t event;

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
		waitpid(pid, &status, 0);

		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		waitpid(pid, &status, 0);
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		/* Fill in the stat structure */
		memset(&buf, 0, sizeof(struct stat));
		buf.st_mode = S_IFCHR;
		buf.st_rdev = 259; /* /dev/null */

		fail_unless(pink_encode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, &buf, sizeof(struct stat)),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_decode_simple(pid, PINKTRACE_DEFAULT_BITNESS, 1, &newbuf, sizeof(struct stat)),
			"%d(%s)", errno, strerror(errno));
		fail_unless(S_ISCHR(newbuf.st_mode), "%d", newbuf.st_mode);
		fail_unless(newbuf.st_rdev == 259, "%d != %d", 259, newbuf.st_rdev);

		pink_trace_kill(pid);
	}
}
END_TEST

Suite *
encode_suite_create(void)
{
	Suite *s = suite_create("encode");

	/* pink_encode_*() */
	TCase *tc_pink_encode = tcase_create("pink_encode");

	tcase_add_test(tc_pink_encode, t_encode_string_first_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_first_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_first_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_string_second_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_second_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_second_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_string_third_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_third_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_third_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_string_fourth_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_fourth_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_fourth_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_string_safe_first_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_first_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_first_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_string_safe_second_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_second_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_second_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_string_safe_third_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_third_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_third_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_string_safe_fourth_lensame);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_fourth_lenshort);
	tcase_add_test(tc_pink_encode, t_encode_string_safe_fourth_lenlong);

	tcase_add_test(tc_pink_encode, t_encode_stat);

	suite_add_tcase(s, tc_pink_encode);

	return s;
}
