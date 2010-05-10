/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon strace which is:
 *   Copyright (c) 1991, 1992 Paul Kranenburg <pk@cs.few.eur.nl>
 *   Copyright (c) 1993 Branko Lankester <branko@hacktic.nl>
 *   Copyright (c) 1993, 1994, 1995, 1996 Rick Sladkey <jrs@world.std.com>
 *   Copyright (c) 1996-1999 Wichert Akkerman <wichert@cistron.nl>
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

#include "check_pinktrace_proc.h"

#include <sys/types.h>

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/pioctl.h>
#include <sys/wait.h>

#include <check.h>
#include <pinktrace/pink.h>

START_TEST(t_get_syscall)
{
	int fd, rfd, flags, status;
	long scno;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_syscall(fd, rfd, PINKTRACE_DEFAULT_BITNESS, &scno),
			"%d(%s)", errno, strerror(errno));
		fail_unless(scno == SYS_getpid, "%ld != %ld", SYS_getpid, scno);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_set_syscall)
{
	int fd, rfd, flags, status;
	long scno;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));
		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);

		/* FIXME: Why is this necessary? */
		if (!ps.state)
			kill(pid, SIGSTOP);

		fail_unless(pink_proc_util_set_syscall(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 0xbadca11),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_proc_util_get_syscall(fd, rfd, PINKTRACE_DEFAULT_BITNESS, &scno),
			"%d(%s)", errno, strerror(errno));
		fail_unless(scno == 0xbadca11, "%ld != %ld", 0xbadca11, scno);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_return_success)
{
	int fd, rfd, flags, status;
	long ret;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCX;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCX, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_return(rfd, &ret),
			"%d(%s)", errno, strerror(errno));
		fail_unless(ret == pid, "%ld != %ld", pid, ret);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_return_fail)
{
	int fd, rfd, flags, status;
	long ret;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		open(NULL, 0); /* Should fail with EFAULT */
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCX;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));
		fail_unless(ps.why == PINK_PROC_EVENT_SCX, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_return(rfd, &ret),
			"%d(%s)", errno, strerror(errno));
		fail_unless(ret == -EFAULT, "%ld != %ld", -EFAULT, ret);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_set_return_success)
{
	int fd, rfd, flags, status;
	long ret;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCX;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));
		fail_unless(ps.why == PINK_PROC_EVENT_SCX, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);

		/* FIXME: Why is this necessary? */
		if (!ps.state)
			kill(pid, SIGSTOP);

		fail_unless(pink_proc_util_set_return(rfd, pid + 1),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_proc_util_get_return(rfd, &ret),
			"%d(%s)", errno, strerror(errno));
		fail_unless(ret == pid + 1, "%ld != %ld", pid + 1, ret);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_set_return_fail)
{
	int fd, rfd, flags, status;
	long ret;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		getpid();
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCX;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));
		fail_unless(ps.why == PINK_PROC_EVENT_SCX, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);

		/* FIXME: Why is this necessary? */
		if (!ps.state)
			kill(pid, SIGSTOP);

		fail_unless(pink_proc_util_set_return(rfd, -ENAMETOOLONG),
			"%d(%s)", errno, strerror(errno));
		fail_unless(pink_proc_util_get_return(rfd, &ret),
			"%d(%s)", errno, strerror(errno));
		fail_unless(ret == -ENAMETOOLONG, "%ld != %ld", -ENAMETOOLONG, ret);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_arg_first)
{
	int fd, rfd, flags, status;
	long arg;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		mmap((void *)13, 0, 0, 0, 0, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_arg(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 0, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "%ld != %ld", 13, arg);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_arg_second)
{
	int fd, rfd, flags, status;
	long arg;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		mmap(NULL, 13, 0, 0, 0, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_arg(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 1, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "%ld != %ld", 13, arg);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_arg_third)
{
	int fd, rfd, flags, status;
	long arg;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 13, 0, 0, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_arg(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 2, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "%ld != %ld", 13, arg);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_arg_fourth)
{
	int fd, rfd, flags, status;
	long arg;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 0, 13, 0, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_arg(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 3, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "%ld != %ld", 13, arg);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_arg_fifth)
{
	int fd, rfd, flags, status;
	long arg;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 0, 0, 13, 0);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_arg(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 4, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "%ld != %ld", 13, arg);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_get_arg_sixth)
{
	int fd, rfd, flags, status;
	long arg;
	pid_t pid;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		mmap(NULL, 0, 0, 0, 0, 13);
	}
	else { /* parent */
		fail_if(waitpid(pid, &status, WUNTRACED) < 0, "%d(%s)", errno, strerror(errno));
		fail_unless(WIFSTOPPED(status), "%#x", status);
		fail_unless(WSTOPSIG(status) == SIGSTOP, "%#x", status);

		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		fail_unless(pink_proc_util_open(pid, &rfd), "%d(%s)", errno, strerror(errno));
		fail_if(rfd < 0, "%d", rfd);

		/* Set event flags */
		flags = PINK_PROC_EVENT_SCE;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));

		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);
		fail_unless(pink_proc_util_get_arg(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 5, &arg),
			"%d(%s)", errno, strerror(errno));
		fail_unless(arg == 13, "%ld != %ld", 13, arg);

		close(fd);
		close(rfd);
	}
}
END_TEST

Suite *
util_suite_create(void)
{
	Suite *s = suite_create("util");

	TCase *tc_pink_proc_util = tcase_create("pink_proc_util");

	tcase_add_test(tc_pink_proc_util, t_get_syscall);
	tcase_add_test(tc_pink_proc_util, t_set_syscall);

	tcase_add_test(tc_pink_proc_util, t_get_return_success);
	tcase_add_test(tc_pink_proc_util, t_get_return_fail);

	tcase_add_test(tc_pink_proc_util, t_set_return_success);
	tcase_add_test(tc_pink_proc_util, t_set_return_fail);

	tcase_add_test(tc_pink_proc_util, t_get_arg_first);
	tcase_add_test(tc_pink_proc_util, t_get_arg_second);
	tcase_add_test(tc_pink_proc_util, t_get_arg_third);
	tcase_add_test(tc_pink_proc_util, t_get_arg_fourth);
	tcase_add_test(tc_pink_proc_util, t_get_arg_fifth);
	tcase_add_test(tc_pink_proc_util, t_get_arg_sixth);

	suite_add_tcase(s, tc_pink_proc_util);

	return s;
}
