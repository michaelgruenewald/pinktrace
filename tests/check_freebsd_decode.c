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
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include "check_pinktrace.h"

#include <pinktrace/pink.h>

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

		buf = pink_decode_string_persistent(pid, PINKTRACE_DEFAULT_BITNESS, 0);
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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 1, &fd, &res),
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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 1, &fd, &res),
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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 1, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_INET, "%d != %d", AF_INET, res.family);

		fail_unless(res.u.sa_in.sin_family == AF_INET, "%d != %d", AF_INET, res.u.sa_in.sin_family);
#if 0
#error FIXME: Why doesnt this work?
		if (!IN_LOOPBACK(res.u.sa_in.sin_addr.s_addr)) {
			inet_ntop(AF_INET, &res.u.sa_in.sin_addr, ip, sizeof(ip));
			fail("INADDR_LOOPBACK != `%s'", ip);
		}
#else
		inet_ntop(AF_INET, &res.u.sa_in.sin_addr, ip, sizeof(ip));
		fail_unless(strcmp(ip, "127.0.0.1") == 0, "INADDR_LOOPBACK != `%s'", ip);
		fail_unless(ntohs(res.u.sa_in.sin_port) == 23456, "23456 != %d", ntohs(res.u.sa_in.sin_port));
#endif

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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 1, &fd, &res),
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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 4, &fd, &res),
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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 4, &fd, &res),
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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 4, &fd, &res),
			"%d(%s)", errno, strerror(errno));
		fail_unless(fd == realfd, "%d != %d", realfd, fd);
		fail_unless(res.family == AF_INET, "%d != %d", AF_INET, res.family);

		fail_unless(res.u.sa_in.sin_family == AF_INET, "%d != %d", AF_INET, res.u.sa_in.sin_family);
#if 0
#error FIXME: Why doesnt this work?
		if (!IN_LOOPBACK(res.u.sa_in.sin_addr.s_addr)) {
			inet_ntop(AF_INET, &res.u.sa_in.sin_addr, ip, sizeof(ip));
			fail("INADDR_LOOPBACK != `%s'", ip);
		}
#else
		inet_ntop(AF_INET, &res.u.sa_in.sin_addr, ip, sizeof(ip));
		fail_unless(strcmp(ip, "127.0.0.1") == 0, "INADDR_LOOPBACK != `%s'", ip);
		fail_unless(ntohs(res.u.sa_in.sin_port) == 23456, "23456 != %d", ntohs(res.u.sa_in.sin_port));
#endif

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
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		/* Resume the child and it will stop at the entry of next system call */
		fail_unless(pink_trace_syscall_entry(pid, 0), "%d(%s)", errno, strerror(errno));

		fail_if(waitpid(pid, &status, 0) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGTRAP, "%#x", status);

		/* Get the file descriptor and compare */
		fail_unless(pink_decode_socket_address(pid, PINKTRACE_DEFAULT_BITNESS, 4, &fd, &res),
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

	tcase_add_test(tc_pink_decode, t_decode_socket_address_null_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet_second);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet6_second);

	tcase_add_test(tc_pink_decode, t_decode_socket_address_null_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_unix_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet_fifth);
	tcase_add_test(tc_pink_decode, t_decode_socket_address_inet6_fifth);

	suite_add_tcase(s, tc_pink_decode);

	return s;
}
