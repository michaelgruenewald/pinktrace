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

/**
 * \file
 * Example describing the low level API
 **/

#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <pinktrace/pink.h>

int
main(int argc, char **argv)
{
	int sig, status;
	long scno;
	const char *scname;
	pid_t pid;
	pink_bitness_t bitness;
	pink_event_t event;
	pink_context_t *ctx;

	/* Parse arguments */
	if (argc < 2) {
		fprintf(stderr, "Usage: %s program [argument...]\n", argv[0]);
		return EXIT_FAILURE;
	}

	/* Create a tracing context. */
	if ((ctx = pink_context_new()) == NULL) {
		fprintf(stderr, "pink_context_new failed: %s\n", strerror(errno));
		return EXIT_FAILURE;
	}

	/* Set tracing options */
	pink_context_set_options(ctx, PINK_TRACE_OPTION_EXEC);

	/* Fork */
	if ((pid = pink_fork(ctx)) < 0) {
		fprintf(stderr, "pink_fork: %s, %s\n",
				pink_error_tostring(pink_context_get_error(ctx)),
				strerror(errno));
		return EXIT_FAILURE;
	}
	else if (!pid) { /* child */
		++argv;
		execvp(argv[0], argv);
		fprintf(stderr, "execvp: %s\n", strerror(errno));
		_exit(-1);
	}
	else {
		/* Figure out the bitness of the process. */
		bitness = pink_bitness_get(pid);
		fprintf(stderr, "Child %i runs in %s mode\n",
				pid, pink_bitness_tostring(bitness));

		sig = 0;
		for (;;) {
			/* At this point child is stopped and needs to be resumed.
			 */
			if (!pink_trace_syscall(pid, sig)) {
				fprintf(stderr, "pink_trace_syscall: %s\n", strerror(errno));
				return (errno == ESRCH) ? 0 : 1;
			}
			sig = 0;

			/* Wait for the child */
			if (waitpid(pid, &status, 0) < 0) {
				fprintf(stderr, "waitpid: %s\n", strerror(errno));
				return (errno == ECHILD) ? 0 : 1;
			}

			/* Check the event. */
			event = pink_event_decide(ctx, status);
			fprintf(stderr, ">>> Received event %d from child %i: %s\n",
					event, pid, pink_event_tostring(event));

			if (event == PINK_EVENT_SYSCALL) {
				/* Get system call number */
				if (!pink_util_get_syscall(pid, &scno))
					fprintf(stderr, "pink_util_get_syscall: %s\n",
							strerror(errno));
				else {
					scname = pink_name_syscall(scno, bitness);
					fprintf(stderr, ">>> Decoded system call %ld, name: %s\n",
							scno, scname ? scname : "unknown");
				}
			}

			/* Send the signal to the child if it was a genuine
			 * signal.
			 */
			if (event == PINK_EVENT_GENUINE || event == PINK_EVENT_UNKNOWN)
				sig = WSTOPSIG(status);
		}

		/* Cleanup and exit */
		pink_context_free(ctx);
		return 0;
	}
}
