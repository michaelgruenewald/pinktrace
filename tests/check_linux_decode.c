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
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <arpa/inet.h>

#include <check.h>

#include <pinktrace/pink.h>

#ifndef INET_ADDRSTRLEN
#define INET_ADDRSTRLEN 16
#endif /* !INET_ADDRSTRLEN */

#if PINKTRACE_HAVE_IPV6
#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif /* !INET6_ADDRSTRLEN */
#endif

/* Some architectures only have SYS_socketcall.
 * For those architectures we need to define SYS_socket so that compiling
 * doesn't fail. */
#ifndef SYS_socket
#define SYS_socket -1
#endif /* !SYS_socket */

/* FIXME: Not sure how portable, these macros are... */
#ifndef IN_LOOPBACK
#define IN_LOOPBACK(a) ((ntohl((a)) >> 24) == 127)
#endif /* !IN_LOOPBACK */

#ifndef IN6_LOOPBACK
#define IN6_LOOPBACK(a)					\
	(((const u_int32_t *) (a))[0] == 0		\
	&& ((const u_int32_t *) (a))[1] == 0		\
	 && ((const u_int32_t *) (a))[2] == 0		\
	 && ((const u_int32_t *) (a))[3] == htonl(1))
#endif /* !IN6_LOOPBACK */

START_TEST(t_decode_stat)
{
	int status;
	struct stat buf;
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the end of next system call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

			/* Make sure we got the right event */
			fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
			event = pink_event_decide(status);
			fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);
		}

		fail_unless(pink_decode_simple(pid, PINKTRACE_BITNESS_DEFAULT, 1, &buf, sizeof(struct stat)),
			"%d(%s)", errno, strerror(errno));
		fail_unless(S_ISCHR(buf.st_mode), "%#x", buf.st_mode);
		fail_unless(buf.st_rdev == 259, "259 != %d", buf.st_rdev);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_first)
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_decode_string(pid, PINKTRACE_BITNESS_DEFAULT, 0, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_second)
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_decode_string(pid, PINKTRACE_BITNESS_DEFAULT, 1, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_third)
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%s", strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_decode_string(pid, PINKTRACE_BITNESS_DEFAULT, 2, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_fourth)
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_decode_string(pid, PINKTRACE_BITNESS_DEFAULT, 3, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_null)
{
	int status;
	char *buf;
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
		open(NULL, O_RDONLY);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		errno = 0;
		buf = pink_decode_string_persistent(pid, PINKTRACE_BITNESS_DEFAULT, 0);
		if (errno)
			fail("%d(%s)", errno, strerror(errno));
		fail_if(buf != NULL, "NULL != `%s'", buf);

		pink_trace_kill(pid);
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

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
		open(notrailingzero, O_RDONLY);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		buf = pink_decode_string_persistent(pid, PINKTRACE_BITNESS_DEFAULT, 0);
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
	char *buf;
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		buf = pink_decode_string_persistent(pid, PINKTRACE_BITNESS_DEFAULT, 0);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		free(buf);
		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_second)
{
	int status;
	char *buf;
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		buf = pink_decode_string_persistent(pid, PINKTRACE_BITNESS_DEFAULT, 1);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		free(buf);
		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_third)
{
	int status;
	char *buf;
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		buf = pink_decode_string_persistent(pid, PINKTRACE_BITNESS_DEFAULT, 2);
		fail_if(buf == NULL, "%d(%s)", strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		free(buf);
		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_fourth)
{
	int status;
	char *buf;
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
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		buf = pink_decode_string_persistent(pid, PINKTRACE_BITNESS_DEFAULT, 3);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		free(buf);
		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_array_member_null)
{
	bool nil;
	int status;
	long arg;
	char buf[1];
	char *const myargv[] = { NULL };
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
		execvp("true", myargv);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 1, &arg),
			"%d(%s)", errno, strerror(errno));

		fail_unless(pink_decode_string_array_member(pid, PINKTRACE_BITNESS_DEFAULT, arg, 0, buf, 1, &nil),
			"%d(%s)", errno, strerror(errno));
		fail_unless(nil, "Not NULL");

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_array_member)
{
	bool nil;
	int status;
	long arg;
	char buf[10];
	char *const myargv[] = { "/dev/null", "/dev/zero", NULL };
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
		execvp("true", myargv);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 1, &arg),
			"%d(%s)", errno, strerror(errno));

		fail_unless(pink_decode_string_array_member(pid, PINKTRACE_BITNESS_DEFAULT, arg, 0, buf, 10, NULL),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);

		fail_unless(pink_decode_string_array_member(pid, PINKTRACE_BITNESS_DEFAULT, arg, 1, buf, 10, NULL),
			"%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/zero", 10), "/dev/zero != `%s'", buf);

		fail_unless(pink_decode_string_array_member(pid, PINKTRACE_BITNESS_DEFAULT, arg, 2, buf, 10, &nil),
			"%d(%s)", errno, strerror(errno));
		fail_unless(nil, "Not NULL");

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_array_member_persistent_null)
{
	int status;
	long arg;
	char *buf;
	char *const myargv[] = { NULL };
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
		execvp("true", myargv);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 1, &arg),
			"%d(%s)", errno, strerror(errno));

		errno = 0;
		buf = pink_decode_string_array_member_persistent(pid, PINKTRACE_BITNESS_DEFAULT, arg, 0);
		if (errno)
			fail("%d(%s)", errno, strerror(errno));
		fail_unless(buf == NULL, "`%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_string_array_member_persistent)
{
	int status;
	long arg;
	char *buf;
	char *const myargv[] = { "/dev/null", "/dev/zero", NULL };
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
		execvp("true", myargv);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_util_get_arg(pid, PINKTRACE_BITNESS_DEFAULT, 1, &arg),
			"%d(%s)", errno, strerror(errno));

		buf = pink_decode_string_array_member_persistent(pid, PINKTRACE_BITNESS_DEFAULT, arg, 0);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/null", 10), "/dev/null != `%s'", buf);
		free(buf);

		buf = pink_decode_string_array_member_persistent(pid, PINKTRACE_BITNESS_DEFAULT, arg, 1);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(0 == strncmp(buf, "/dev/zero", 10), "/dev/zero != `%s'", buf);
		free(buf);

		errno = 0;
		buf = pink_decode_string_array_member_persistent(pid, PINKTRACE_BITNESS_DEFAULT, arg, 2);
		if (errno)
			fail("%d(%s)", errno, strerror(errno));
		fail_unless(buf == NULL, "`%s'", buf);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_call)
{
	int status;
	long scall;
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
		socket(AF_UNIX, SOCK_STREAM, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);
		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child and it will stop at the next system call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		/* Make sure we got the right event */
		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		event = pink_event_decide(status);
		fail_unless(event == PINK_EVENT_SYSCALL, "%d != %d", PINK_EVENT_SYSCALL, event);

		fail_unless(pink_decode_socket_call(pid, PINKTRACE_BITNESS_DEFAULT, &scall), "%d(%s)",
			errno, strerror(errno));
#if defined(SYS_socketcall)
		fail_unless(scall == PINK_SOCKET_SUBCALL_SOCKET, "%d != %ld", PINK_SOCKET_SUBCALL_SOCKET, scall);
#else
		fail_unless(scall == SYS_socket, "%d != %ld", SYS_socket, scall);
#endif /* defined(SYS_socketcall) */

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_fd)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_un addr;

		close(pfd[0]);

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
			fprintf(stderr, "socket: %s\n", strerror(errno));
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "/dev/null");

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		connect(fd, (struct sockaddr *)&addr, SUN_LEN(&addr));
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%d != %d", SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the connect() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);


		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_fd(pid, PINKTRACE_BITNESS_DEFAULT, 0, &fd),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_address_null_second)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		close(pfd[0]);

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		connect(fd, NULL, 0);
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the connect() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 1, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == -1, "-1 != %d", res.family);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_address_unix_second)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_un addr;

		close(pfd[0]);

		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "/dev/null");

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		connect(fd, (struct sockaddr *)&addr, SUN_LEN(&addr));
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the connect() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 1, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_UNIX, "%d != %d", AF_UNIX, res.family);

		fail_unless(res.u.sa_un.sun_family == AF_UNIX, "%d != %d", AF_UNIX, res.u.sa_un.sun_family);
		fail_unless(strncmp(res.u.sa_un.sun_path, "/dev/null", 10) == 0,
			"/dev/null != `%s'", res.u.sa_un.sun_path);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_address_unix_abstract_second)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		size_t len;
		struct sockaddr_un addr;

		close(pfd[0]);

		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "X/dev/null");
		len = SUN_LEN(&addr);
		/* Make the socket abstract */
		addr.sun_path[0] = '\0';

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		connect(fd, (struct sockaddr *)&addr, len);
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the connect() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 1, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_UNIX, "%d != %d", AF_UNIX, res.family);

		fail_unless(res.u.sa_un.sun_family == AF_UNIX, "%d != %d", AF_UNIX, res.u.sa_un.sun_family);
		fail_unless(res.u.sa_un.sun_path[0] == '\0', "0 != `%c'", res.u.sa_un.sun_path[0]);
		fail_unless(strncmp(res.u.sa_un.sun_path + 1, "/dev/null", 10) == 0,
			"/dev/null != `%s'", res.u.sa_un.sun_path + 1);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_address_inet_second)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_in addr;

		close(pfd[0]);

		addr.sin_family = AF_INET;
		addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
		addr.sin_port = htons(23456);

		if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		connect(fd, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;
		char ip[INET_ADDRSTRLEN];

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the connect() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 1, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_INET, "%d != %d", AF_INET, res.family);

		fail_unless(res.u.sa_in.sin_family == AF_INET, "%d != %d", AF_INET, res.u.sa_in.sin_family);
		if (!IN_LOOPBACK(res.u.sa_in.sin_addr.s_addr)) {
			inet_ntop(AF_INET, &res.u.sa_in.sin_addr, ip, sizeof(ip));
			fail("INADDR_LOOPBACK != `%s'", ip);
		}
		fail_unless(ntohs(res.u.sa_in.sin_port) == 23456, "23456 != %d", ntohs(res.u.sa_in.sin_port));

		pink_trace_kill(pid);
	}
}
END_TEST

#if PINKTRACE_HAVE_IPV6
START_TEST(t_decode_socket_address_inet6_second)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_in6 addr;

		close(pfd[0]);

		addr.sin6_family = AF_INET6;
		addr.sin6_addr = in6addr_loopback;
		addr.sin6_port = htons(23456);

		if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			fprintf(stderr, "socket: %s\n", strerror(errno));
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			fprintf(stderr, "pink_trace_me: %s\n", strerror(errno));
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		connect(fd, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;
		char ip[INET6_ADDRSTRLEN];

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the connect() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 1, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_INET6, "%d != %d", AF_INET6, res.family);

		fail_unless(res.u.sa6.sin6_family == AF_INET6, "%d != %d", AF_INET6, res.u.sa6.sin6_family);
		if (!IN6_LOOPBACK(res.u.sa6.sin6_addr.s6_addr)) {
			inet_ntop(AF_INET6, &res.u.sa6.sin6_addr, ip, sizeof(ip));
			fail("in6addr_loopback != `%s'", ip);
		}
		fail_unless(ntohs(res.u.sa6.sin6_port) == 23456, "23456 != %d", ntohs(res.u.sa6.sin6_port));

		pink_trace_kill(pid);
	}
}
END_TEST
#endif /* PINKTRACE_HAVE_IPV6 */

START_TEST(t_decode_socket_address_null_fifth)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		close(pfd[0]);

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		sendto(fd, NULL, 0, 0, NULL, 0);
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the sendto() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 4, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == -1, "-1 != %d", res.family);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_address_unix_fifth)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_un addr;

		close(pfd[0]);

		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "/dev/null");

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, SUN_LEN(&addr));
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the sendto() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 4, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_UNIX, "%d != %d", AF_UNIX, res.family);

		fail_unless(res.u.sa_un.sun_family == AF_UNIX, "%d != %d", AF_UNIX, res.u.sa_un.sun_family);
		fail_unless(strncmp(res.u.sa_un.sun_path, "/dev/null", 10) == 0,
			"/dev/null != `%s'", res.u.sa_un.sun_path);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_address_unix_abstract_fifth)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		size_t len;
		struct sockaddr_un addr;

		close(pfd[0]);

		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "X/dev/null");
		len = SUN_LEN(&addr);
		/* Make the socket abstract */
		addr.sun_path[0] = '\0';

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, len);
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the sendto() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 4, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_UNIX, "%d != %d", AF_UNIX, res.family);

		fail_unless(res.u.sa_un.sun_family == AF_UNIX, "%d != %d", AF_UNIX, res.u.sa_un.sun_family);
		fail_unless(res.u.sa_un.sun_path[0] == '\0', "0 != `%c'", res.u.sa_un.sun_path[0]);
		fail_unless(strncmp(res.u.sa_un.sun_path + 1, "/dev/null", 10) == 0,
			"/dev/null != `%s'", res.u.sa_un.sun_path + 1);

		pink_trace_kill(pid);
	}
}
END_TEST

START_TEST(t_decode_socket_address_inet_fifth)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_in addr;

		close(pfd[0]);

		addr.sin_family = AF_INET;
		addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
		addr.sin_port = htons(23456);

		if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;
		char ip[INET_ADDRSTRLEN];

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the sendto() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 4, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_INET, "%d != %d", AF_INET, res.family);

		fail_unless(res.u.sa_in.sin_family == AF_INET, "%d != %d", AF_INET, res.u.sa_in.sin_family);
		if (!IN_LOOPBACK(res.u.sa_in.sin_addr.s_addr)) {
			inet_ntop(AF_INET, &res.u.sa_in.sin_addr, ip, sizeof(ip));
			fail("INADDR_LOOPBACK != `%s'", ip);
		}
		fail_unless(ntohs(res.u.sa_in.sin_port) == 23456, "23456 != %d", ntohs(res.u.sa_in.sin_port));

		pink_trace_kill(pid);
	}
}
END_TEST

#if PINKTRACE_HAVE_IPV6
START_TEST(t_decode_socket_address_inet6_fifth)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_socket_address_t res;

	if (pipe(pfd) < 0)
		fail("pipe: %d(%s)", errno, strerror(errno));

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_in6 addr;

		close(pfd[0]);

		addr.sin6_family = AF_INET6;
		addr.sin6_addr = in6addr_loopback;
		addr.sin6_port = htons(23456);

		if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			perror("socket");
			_exit(EXIT_FAILURE);
		}

		snprintf(strfd, 16, "%i", (int)fd);
		write(pfd[1], strfd, 16);
		close(pfd[1]);

		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}

		kill(getpid(), SIGSTOP);
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;
		char ip[INET6_ADDRSTRLEN];

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_if(WIFEXITED(status), "%#x", status);
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD), "%d(%s)", errno, strerror(errno));

		/* Resume the child, until the sendto() call */
		fail_unless(pink_trace_syscall(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_BITNESS_DEFAULT, 4, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_INET6, "%d != %d", AF_INET6, res.family);

		fail_unless(res.u.sa6.sin6_family == AF_INET6, "%d != %d", AF_INET6, res.u.sa6.sin6_family);
		if (!IN6_LOOPBACK(res.u.sa6.sin6_addr.s6_addr)) {
			inet_ntop(AF_INET6, &res.u.sa6.sin6_addr, ip, sizeof(ip));
			fail("in6addr_loopback != `%s'", ip);
		}
		fail_unless(ntohs(res.u.sa6.sin6_port) == 23456, "23456 != %d", ntohs(res.u.sa6.sin6_port));

		pink_trace_kill(pid);
	}
}
END_TEST
#endif /* PINKTRACE_HAVE_IPV6 */

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

	tcase_add_test(tc_pink_decode, t_decode_string_array_member_null);
	tcase_add_test(tc_pink_decode, t_decode_string_array_member);
	tcase_add_test(tc_pink_decode, t_decode_string_array_member_persistent_null);
	tcase_add_test(tc_pink_decode, t_decode_string_array_member_persistent);

	tcase_add_test(tc_pink_decode, t_decode_socket_call);
	tcase_add_test(tc_pink_decode, t_decode_socket_fd);

	tcase_add_test(tc_pink_decode, t_decode_socket_address_null_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_abstract_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet_second);
#if PINKTRACE_HAVE_IPV6
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet6_second);
#endif /* PINKTRACE_HAVE_IPV6 */

	tcase_add_test(tc_pink_decode, t_decode_socket_address_null_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_abstract_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet_fifth);
#if PINKTRACE_HAVE_IPV6
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet6_fifth);
#endif /* PINKTRACE_HAVE_IPV6 */

	suite_add_tcase(s, tc_pink_decode);

	return s;
}
