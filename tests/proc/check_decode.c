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

#include "check_pinktrace_proc.h"

#include <sys/types.h>

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/pioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <check.h>
#include <pinktrace/pink.h>

START_TEST(t_decode_stat)
{
	int fd, rfd, flags, status;
	unsigned long arg;
	pid_t pid;
	struct procfs_status ps;
	struct stat buf;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		stat("/dev/null", &buf);
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
		flags = PINK_PROC_EVENT_SCE | PINK_PROC_EVENT_SCX;
		fail_unless(pink_proc_set_event_flags(fd, flags), "%d(%s)", errno, strerror(errno));

		/* Let the child continue */
		kill(pid, SIGCONT);
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));
		fail_unless(ps.why == PINK_PROC_EVENT_SCE, "%#x != %#x", PINK_PROC_EVENT_SCE, ps.why);

		/* FIXME: Why is this necessary? */
		if (!ps.state)
			kill(pid, SIGSTOP);

		fail_unless(pink_proc_util_get_arg(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 1, &arg),
			"%d(%s)", errno, strerror(errno));

		fail_unless(pink_proc_cont(fd), "%d(%s)", errno, strerror(errno));
		fail_unless(pink_proc_wait(fd, &ps), "%d(%s)", errno, strerror(errno));
		fail_unless(ps.why == PINK_PROC_EVENT_SCX, "%#x != %#x", PINK_PROC_EVENT_SCX, ps.why);

		fail_unless(pink_proc_read(fd, arg, &buf, sizeof(buf)), "%d(%s)", errno, strerror(errno));
		fail_unless(S_ISCHR(buf.st_mode), "%#x", buf.st_mode);

		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_first)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char buf[10];
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		open("/dev/null", O_RDONLY);
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

		fail_unless(pink_proc_decode_string(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 0, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "/dev/null != `%s'", buf);

		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_second)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char buf[10];
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		openat(-1, "/dev/null", O_RDONLY);
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

		fail_unless(pink_proc_decode_string(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 1, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "/dev/null != `%s'", buf);

		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_third)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char buf[10];
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		symlinkat("/var/empty", AT_FDCWD, "/dev/null");
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

		fail_unless(pink_proc_decode_string(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 2, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "/dev/null != `%s'", buf);

		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_fourth)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char buf[10];
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "/dev/null", 0600);
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

		fail_unless(pink_proc_decode_string(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 3, buf, 10),
			"%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "/dev/null != `%s'", buf);

		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_null)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char *buf;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		open(NULL, O_RDONLY);
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

		buf = pink_proc_decode_string_persistent(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 0);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "", 1) == 0, "`' != `%s'", buf);

		free(buf);
		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

#if 0
#error FIXME: This tests needs some love!
START_TEST(t_decode_string_persistent_notrailingzero)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char notrailingzero[3] = {'n', 'i', 'l'};
	char *buf;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		open(notrailingzero, O_RDONLY);
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

		buf = pink_proc_decode_string_persistent(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 0);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(buf[0] == 'n', "n != %c", buf[0]);
		fail_unless(buf[1] == 'i', "i != %c", buf[1]);
		fail_unless(buf[2] == 'l', "l != %c", buf[2]);

		free(buf);
		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST
#endif

START_TEST(t_decode_string_persistent_first)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char *buf;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		open("/dev/null", O_RDONLY);
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

		buf = pink_proc_decode_string_persistent(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 0);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "/dev/null != `%s'", buf);

		free(buf);
		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_second)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char *buf;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		openat(-1, "/dev/null", O_RDONLY);
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

		buf = pink_proc_decode_string_persistent(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 1);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "/dev/null != `%s'", buf);

		free(buf);
		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_third)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char *buf;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		symlinkat("/var/empty", AT_FDCWD, "/dev/null");
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

		buf = pink_proc_decode_string_persistent(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 2);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "/dev/null != `%s'", buf);

		free(buf);
		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

START_TEST(t_decode_string_persistent_fourth)
{
	int fd, rfd, flags, status;
	pid_t pid;
	char *buf;
	struct procfs_status ps;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) { /* child */
		kill(getpid(), SIGSTOP);
		linkat(AT_FDCWD, "/var/empty", AT_FDCWD, "/dev/null", 0600);
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

		buf = pink_proc_decode_string_persistent(fd, rfd, PINKTRACE_DEFAULT_BITNESS, 3);
		fail_if(buf == NULL, "%d(%s)", errno, strerror(errno));
		fail_unless(strncmp(buf, "/dev/null", 10) == 0, "`' != `%s'", buf);

		free(buf);
		kill(pid, SIGKILL);
		close(fd);
		close(rfd);
	}
}
END_TEST

Suite *
decode_suite_create(void)
{
	Suite *s = suite_create("decode");

	TCase *tc_pink_proc_decode = tcase_create("pink_proc_decode");

	tcase_add_test(tc_pink_proc_decode, t_decode_stat);

	tcase_add_test(tc_pink_proc_decode, t_decode_string_first);
	tcase_add_test(tc_pink_proc_decode, t_decode_string_second);
	tcase_add_test(tc_pink_proc_decode, t_decode_string_third);
	tcase_add_test(tc_pink_proc_decode, t_decode_string_fourth);

	tcase_add_test(tc_pink_proc_decode, t_decode_string_persistent_null);
	/* tcase_add_test(tc_pink_proc_decode, t_decode_string_persistent_notrailingzero); */
	tcase_add_test(tc_pink_proc_decode, t_decode_string_persistent_first);
	tcase_add_test(tc_pink_proc_decode, t_decode_string_persistent_second);
	tcase_add_test(tc_pink_proc_decode, t_decode_string_persistent_third);
	tcase_add_test(tc_pink_proc_decode, t_decode_string_persistent_fourth);

	suite_add_tcase(s, tc_pink_proc_decode);

	return s;
}
