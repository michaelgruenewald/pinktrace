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

#include "check_pinktrace_ptrace.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace/pink.h>

START_TEST(t_bitness)
{
	int status;
	pid_t pid;
	pink_bitness_t bitness;

	if ((pid = fork()) < 0)
		fail("fork: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(-1);
		}
		kill(getpid(), SIGSTOP);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		bitness = pink_trace_util_get_bitness(pid);
		fail_unless(bitness == PINKTRACE_DEFAULT_BITNESS,
				"Wrong bitness, expected: %d got: %d",
				PINKTRACE_DEFAULT_BITNESS, bitness);

		pink_trace_kill(pid);
	}
}
END_TEST

Suite *
bitness_suite_create(void)
{
	Suite *s = suite_create("bitness");

	/* pink_util_get_bitness() */
	TCase *tc_pink_bitness = tcase_create("pink_trace_bitness");

	tcase_add_test(tc_pink_bitness, t_bitness);

	suite_add_tcase(s, tc_pink_bitness);

	return s;
}
