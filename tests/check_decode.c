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

/* FIXME: We use a netlink socket as the unsupported family which is NOT
 * portable.
 */
#include <linux/netlink.h>

#include <check.h>

#include <pinktrace/pink.h>

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
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		_exit((0 > stat("/dev/null", &buf)) ? EXIT_FAILURE : EXIT_SUCCESS);
	else { /* parent */
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

		fail_unless(pink_decode_simple(pid, CHECK_BITNESS, 1, &buf, sizeof(struct stat)),
				"pink_decode_simple: %s",
				strerror(errno));
		fail_unless(S_ISCHR(buf.st_mode), "Not a character device: %#x", buf.st_mode);
		fail_unless(buf.st_rdev == 259, "Wrong device ID, expected: %d got: %d",
				259, buf.st_rdev);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
	}
}
END_TEST

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

START_TEST(t_decode_socket_call)
{
	bool decoded;
	int status;
	long scall;
	pid_t pid;
	pink_event_t event;
	pink_context_t *ctx;

	ctx = pink_context_new();
	fail_unless(ctx != NULL, "pink_context_new failed: %s", strerror(errno));

	if ((pid = pink_fork(ctx)) < 0)
		fail("pink_fork: %s (%s)", pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
	else if (!pid) /* child */
		socket(AF_UNIX, SOCK_STREAM, 0);
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

		fail_unless(pink_decode_socket_call(pid, CHECK_BITNESS, &scall, &decoded),
				"pink_decode_socket_call: %s", strerror(errno));
		if (decoded)
			fail_unless(scall == PINK_SOCKET_SUBCALL_SOCKET,
				"Wrong decoded subcall, expected: %d got: %ld",
				PINK_SOCKET_SUBCALL_SOCKET, scall);
		else
			fail_unless(scall == SYS_socket, "Wrong decoded subcall, expected: %d got: %ld",
					SYS_socket, scall);

		pink_context_free(ctx);
		kill(pid, SIGKILL);
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
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
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
			fprintf(stderr, "pink_trace_me: %s\n", strerror(errno));
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

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_fd(pid, CHECK_BITNESS, 0, &fd),
				"pink_decode_socket_fd: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);

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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		close(pfd[0]);

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
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
		connect(fd, NULL, 0);
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 1, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == -1,
				"Wrong family, expected: -1 got: %d",
				pink_sockaddr_get_family(res));

		pink_sockaddr_free(res);
		pink_trace_kill(pid);
	}
}
END_TEST

/* FIXME: This test case uses a netlink socket which is NOT portable. */
START_TEST(t_decode_socket_address_unknown_second)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_nl addr;

		close(pfd[0]);

		memset(&addr, 0, sizeof(addr));
		addr.nl_family = AF_NETLINK;
		addr.nl_pid = getpid();
		addr.nl_groups = 0;

		if ((fd = socket(AF_NETLINK, SOCK_RAW, 0)) < 0) {
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
		bind(fd, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 1, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == -1,
				"Wrong family, expected: -1 got: %d",
				pink_sockaddr_get_family(res));

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_un addr;

		close(pfd[0]);

		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "/dev/null");

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
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
		connect(fd, (struct sockaddr *)&addr, SUN_LEN(&addr));
	}
	else { /* parent */
		int realfd;
		const struct sockaddr_un *unaddr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 1, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_UNIX,
				"Wrong family, expected: %d got: %d",
				AF_UNIX, pink_sockaddr_get_family(res));

		unaddr = pink_sockaddr_get_unix(res);
		fail_if(unaddr == NULL, "pink_sockaddr_get_unix barfed!");
		fail_unless(unaddr->sun_family == AF_UNIX, "Wrong family, expected: %d got: %d",
				AF_UNIX, unaddr->sun_family);
		fail_unless(strncmp(unaddr->sun_path, "/dev/null", 10) == 0,
				"Wrong path, expected: /dev/null got: `%s'",
				unaddr->sun_path);

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
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
		connect(fd, (struct sockaddr *)&addr, len);
	}
	else { /* parent */
		int realfd;
		const struct sockaddr_un *unaddr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 1, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_UNIX,
				"Wrong family, expected: %d got: %d",
				AF_UNIX, pink_sockaddr_get_family(res));

		unaddr = pink_sockaddr_get_unix(res);
		fail_if(unaddr == NULL, "pink_sockaddr_get_unix barfed!");
		fail_unless(unaddr->sun_family == AF_UNIX, "Wrong family, expected: %d got: %d",
				AF_UNIX, unaddr->sun_family);
		fail_unless(unaddr->sun_path[0] == '\0', "Wrong initial char, expected: NULL got: `%c'",
				unaddr->sun_path[0]);
		fail_unless(strncmp(unaddr->sun_path + 1, "/dev/null", 10) == 0,
				"Wrong path, expected: /dev/null got: `%s'",
				unaddr->sun_path + 1);

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_in addr;

		close(pfd[0]);

		addr.sin_family = AF_INET;
		addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
		addr.sin_port = htons(23456);

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
		char ip[100] = { 0 };
		const struct sockaddr_in *inaddr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 1, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_INET,
				"Wrong family, expected: %d got: %d",
				AF_INET, pink_sockaddr_get_family(res));

		inaddr = pink_sockaddr_get_inet(res);
		fail_if(inaddr == NULL, "pink_sockaddr_get_inet barfed!");
		fail_unless(inaddr->sin_family == AF_INET,
				"Wrong family, expected: %d got: %d",
				AF_INET, inaddr->sin_family);
		if (!IN_LOOPBACK(inaddr->sin_addr.s_addr)) {
			inet_ntop(AF_INET, &inaddr->sin_addr, ip, sizeof(ip));
			fail("Wrong address, expected: INADDR_LOOPBACK got: `%s'", ip);
		}
		fail_unless(ntohs(inaddr->sin_port) == 23456,
				"Wrong port, expected: 23456 got: %d",
				ntohs(inaddr->sin_port));

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
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
		char ip[100] = { 0 };
		const struct sockaddr_in6 *in6addr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 1, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_INET6,
				"Wrong family, expected: %d got: %d",
				AF_INET6, pink_sockaddr_get_family(res));

		in6addr = pink_sockaddr_get_inet6(res);
		fail_if(in6addr == NULL, "pink_sockaddr_get_inet6 barfed!");
		fail_unless(in6addr->sin6_family == AF_INET6,
				"Wrong family, expected: %d got: %d",
				AF_INET6, in6addr->sin6_family);
		if (!IN6_LOOPBACK(in6addr->sin6_addr.s6_addr)) {
			inet_ntop(AF_INET6, &in6addr->sin6_addr, ip, sizeof(ip));
			fail("Wrong address, expected: in6addr_loopback got: `%s'", ip);
		}
		fail_unless(ntohs(in6addr->sin6_port) == 23456,
				"Wrong port, expected: 23456 got: %d",
				ntohs(in6addr->sin6_port));

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		close(pfd[0]);

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
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
		sendto(fd, NULL, 0, 0, NULL, 0);
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 4, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == -1,
				"Wrong family, expected: -1 got: %d",
				pink_sockaddr_get_family(res));

		pink_sockaddr_free(res);
		pink_trace_kill(pid);
	}
}
END_TEST

/* FIXME: This test case uses a netlink socket which is NOT portable. */
START_TEST(t_decode_socket_address_unknown_fifth)
{
	int status;
	long fd;
	int pfd[2];
	char strfd[16];
	pid_t pid;
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_nl addr;

		close(pfd[0]);

		memset(&addr, 0, sizeof(addr));
		addr.nl_family = AF_NETLINK;
		addr.nl_pid = getpid();
		addr.nl_groups = 0;

		if ((fd = socket(AF_NETLINK, SOCK_RAW, 0)) < 0) {
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
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 4, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == -1,
				"Wrong family, expected: -1 got: %d",
				pink_sockaddr_get_family(res));

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_un addr;

		close(pfd[0]);

		addr.sun_family = AF_UNIX;
		strcpy(addr.sun_path, "/dev/null");

		if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
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
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, SUN_LEN(&addr));
	}
	else { /* parent */
		int realfd;
		const struct sockaddr_un *unaddr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 4, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_UNIX,
				"Wrong family, expected: %d got: %d",
				AF_UNIX, pink_sockaddr_get_family(res));

		unaddr = pink_sockaddr_get_unix(res);
		fail_if(unaddr == NULL, "pink_sockaddr_get_unix barfed!");
		fail_unless(unaddr->sun_family == AF_UNIX, "Wrong family, expected: %d got: %d",
				AF_UNIX, unaddr->sun_family);
		fail_unless(strncmp(unaddr->sun_path, "/dev/null", 10) == 0,
				"Wrong path, expected: /dev/null got: `%s'",
				unaddr->sun_path);

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
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
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, len);
	}
	else { /* parent */
		int realfd;
		const struct sockaddr_un *unaddr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 4, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_UNIX,
				"Wrong family, expected: %d got: %d",
				AF_UNIX, pink_sockaddr_get_family(res));

		unaddr = pink_sockaddr_get_unix(res);
		fail_if(unaddr == NULL, "pink_sockaddr_get_unix barfed!");
		fail_unless(unaddr->sun_family == AF_UNIX, "Wrong family, expected: %d got: %d",
				AF_UNIX, unaddr->sun_family);
		fail_unless(unaddr->sun_path[0] == '\0', "Wrong initial char, expected: NULL got: `%c'",
				unaddr->sun_path[0]);
		fail_unless(strncmp(unaddr->sun_path + 1, "/dev/null", 10) == 0,
				"Wrong path, expected: /dev/null got: `%s'",
				unaddr->sun_path + 1);

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		struct sockaddr_in addr;

		close(pfd[0]);

		addr.sin_family = AF_INET;
		addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
		addr.sin_port = htons(23456);

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
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;
		char ip[100] = { 0 };
		const struct sockaddr_in *inaddr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 4, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_INET,
				"Wrong family, expected: %d got: %d",
				AF_INET, pink_sockaddr_get_family(res));

		inaddr = pink_sockaddr_get_inet(res);
		fail_if(inaddr == NULL, "pink_sockaddr_get_inet barfed!");
		fail_unless(inaddr->sin_family == AF_INET,
				"Wrong family, expected: %d got: %d",
				AF_INET, inaddr->sin_family);
		if (!IN_LOOPBACK(inaddr->sin_addr.s_addr)) {
			inet_ntop(AF_INET, &inaddr->sin_addr, ip, sizeof(ip));
			fail("Wrong address, expected: INADDR_LOOPBACK got: `%s'", ip);
		}
		fail_unless(ntohs(inaddr->sin_port) == 23456,
				"Wrong port, expected: 23456 got: %d",
				ntohs(inaddr->sin_port));

		pink_sockaddr_free(res);
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
	pink_sockaddr_t *res;

	if (pipe(pfd) < 0)
		fail("pipe: %s", strerror(errno));

	/* We don't use pink_fork() for this test because the child needs to
	 * write the file descriptor to a pipe. */
	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
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
		sendto(fd, NULL, 0, 0, (struct sockaddr *)&addr, sizeof(addr));
	}
	else { /* parent */
		int realfd;
		char ip[100] = { 0 };
		const struct sockaddr_in6 *in6addr;

		close(pfd[1]);

		read(pfd[0], strfd, 16);
		realfd = atoi(strfd);
		close(pfd[0]);

		waitpid(pid, &status, 0);
		fail_if(WIFEXITED(status), "Child exited with code %d", WEXITSTATUS(status));
		fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		fail_unless(WSTOPSIG(status) == SIGSTOP, "Wrong signal, expected: %d got: %d",
				SIGSTOP, WSTOPSIG(status));

		fail_unless(pink_trace_setup(pid, PINK_TRACE_OPTION_SYSGOOD),
				"pink_trace_setup: %s", strerror(errno));

		/* Resume the child, until the connect() call */
		for (unsigned int i = 0; i < 2; i++) {
			fail_unless(pink_trace_syscall(pid, 0),
					"pink_trace_syscall: %s",
					strerror(errno));

			waitpid(pid, &status, 0);
			fail_unless(WIFSTOPPED(status), "Child hasn't stopped");
		}

		/* Get the file descriptor and compare */
		res = pink_decode_socket_address(pid, CHECK_BITNESS, 4, &fd);
		fail_if(res == NULL, "pink_decode_socket_address: %s", strerror(errno));
		fail_unless(fd == realfd, "Wrong file descriptor, expected: %d got: %d",
				realfd, fd);
		fail_unless(pink_sockaddr_get_family(res) == AF_INET6,
				"Wrong family, expected: %d got: %d",
				AF_INET6, pink_sockaddr_get_family(res));

		in6addr = pink_sockaddr_get_inet6(res);
		fail_if(in6addr == NULL, "pink_sockaddr_get_inet6 barfed!");
		fail_unless(in6addr->sin6_family == AF_INET6,
				"Wrong family, expected: %d got: %d",
				AF_INET6, in6addr->sin6_family);
		if (!IN6_LOOPBACK(in6addr->sin6_addr.s6_addr)) {
			inet_ntop(AF_INET6, &in6addr->sin6_addr, ip, sizeof(ip));
			fail("Wrong address, expected: in6addr_loopback got: `%s'", ip);
		}
		fail_unless(ntohs(in6addr->sin6_port) == 23456,
				"Wrong port, expected: 23456 got: %d",
				ntohs(in6addr->sin6_port));

		pink_sockaddr_free(res);
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

	tcase_add_test(tc_pink_decode, t_decode_socket_call);
	tcase_add_test(tc_pink_decode, t_decode_socket_fd);

	tcase_add_test(tc_pink_decode, t_decode_socket_address_null_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unknown_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_abstract_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet_second);
#if PINKTRACE_HAVE_IPV6
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet6_second);
#endif /* PINKTRACE_HAVE_IPV6 */

	tcase_add_test(tc_pink_decode, t_decode_socket_address_null_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unknown_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_abstract_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet_fifth);
#if PINKTRACE_HAVE_IPV6
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet6_fifth);
#endif /* PINKTRACE_HAVE_IPV6 */

	suite_add_tcase(s, tc_pink_decode);

	return s;
}
