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
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include <check.h>
#include <pinktrace/pink.h>

START_TEST(t_proc_open_curproc)
{
	int fd;

	fail_unless(pink_proc_open(-1, &fd), "%d(%s)", errno, strerror(errno));
	fail_if(fd < 0, "%d", fd);

	close(fd);
}
END_TEST

START_TEST(t_proc_open)
{
	int fd;
	pid_t pid;

	if ((pid = fork()) < 0)
		fail("fork: %d(%s)", errno, strerror(errno));
	else if (!pid) /* child */
		pause();
	else { /* parent */
		fail_unless(pink_proc_open(pid, &fd), "%d(%s)", errno, strerror(errno));
		fail_if(fd < 0, "%d", fd);

		close(fd);
		kill(pid, SIGKILL);
	}
}
END_TEST

Suite *
fs_suite_create(void)
{
	Suite *s = suite_create("fs");

	TCase *tc_pink_proc_fs = tcase_create("pink_proc_fs");

	tcase_add_test(tc_pink_proc_fs, t_proc_open_curproc);
	tcase_add_test(tc_pink_proc_fs, t_proc_open);

	suite_add_tcase(s, tc_pink_proc_fs);

	return s;
}
