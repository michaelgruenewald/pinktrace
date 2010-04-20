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

#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <check.h>

#include <pinktrace.h>

START_TEST(test_pink_trace_me_basic)
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
		_exit(EXIT_SUCCESS);
	}
	else { /* parent */
		waitpid(pid, &status, 0);
		fail_unless(WEXITSTATUS(status) == EXIT_SUCCESS, "pink_trace_me() failed");
	}
}
END_TEST

START_TEST(test_pink_trace_me_signal)
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
		kill(getpid(), SIGTTIN);
		abort();
	}
	else { /* parent */
		waitpid(pid, &status, 0);
		fail_unless(WIFSTOPPED(status), "Child hasn't been stopped after genuine signal");
		kill(pid, SIGKILL);
	}
}
END_TEST

START_TEST(test_pink_trace_me_execve)
{
	int status;
	pid_t pid;
	char *const myargv[] = { "/bin/true", NULL };

	if ((pid = fork()) < 0)
		fail("fork failed: %s", strerror(errno));
	else if (!pid) { /* child */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		execvp("/bin/true", myargv);
		_exit(EXIT_FAILURE);
	}
	else { /* parent */
		waitpid(pid, &status, 0);
		fail_unless(WIFSTOPPED(status), "Child hasn't been stopped after execve()");
		fail_unless(WSTOPSIG(status) == SIGTRAP, "Wrong signal, got: %d expected: %d",
				WSTOPSIG(status), SIGTRAP);
		kill(pid, SIGKILL);
	}
}
END_TEST

Suite *
pinktrace_suite(void)
{
	Suite *s = suite_create("pinktrace");

	TCase *tc_core = tcase_create("Core");

	tcase_add_test(tc_core, test_pink_trace_me_basic);
	tcase_add_test(tc_core, test_pink_trace_me_signal);
	tcase_add_test(tc_core, test_pink_trace_me_execve);

	suite_add_tcase(s, tc_core);

	return s;
}

int
main(void)
{
	int number_failed;
	Suite *s;
	SRunner *sr;

	s = pinktrace_suite();
	sr = srunner_create(s);
	srunner_run_all(sr, CK_VERBOSE);

	number_failed = srunner_ntests_failed(sr);
	srunner_free(sr);
	return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
