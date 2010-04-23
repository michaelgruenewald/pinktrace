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
 * Example describing the high level API
 **/

#include <errno.h>
#include <stdbool.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <pinktrace/pink.h>

/* Child function */
static int
child_func(void *userdata)
{
	char **myargv = (char **)userdata;

	execvp(myargv[0], myargv);
	fprintf(stderr, "execvp: %s\n", strerror(errno));
	return -1;
}

/* Event handlers */
static bool
cb_syscall(pink_unused const pink_context_t *ctx, pid_t pid, pink_unused void *userdata)
{
	fprintf(stderr, ">>> Child %i has entered a system call!\n", pid);
	return pink_trace_syscall(pid, 0);
}

static bool
cb_exec(pink_unused const pink_context_t *ctx, pid_t pid, pink_unused void *userdata)
{
	fprintf(stderr, ">>> Child %i called execve()\n", pid);
	return pink_trace_syscall(pid, 0);
}

/* Error handler */
static pink_error_return_t
cb_error(const pink_context_t *ctx, pid_t pid, pink_unused void *userdata)
{
	pink_error_t err;
	pink_error_return_t ret;

	err = pink_context_get_error(ctx);
	fprintf(stderr, "Loop error (child[%i]): %s (%s)\n", pid,
		pink_error_tostring(err), strerror(errno));

	ret.fatal = true;
	ret.code = EXIT_FAILURE;

	return ret;
}

int
main(int argc, char **argv)
{
	int ret;
	pink_context_t *ctx;
	pink_event_handler_t *handler;

	/* Parse arguments */
	if (argc < 2) {
		fprintf(stderr, "Usage: %s program [argument...]\n", argv[0]);
		return EXIT_FAILURE;
	}

	/* Create a tracing context. */
	ctx = pink_context_new();
	if (!ctx) {
		fprintf(stderr, "pink_context_new: %s\n", strerror(errno));
		return EXIT_FAILURE;
	}

	/* Set context options */
	pink_context_set_step(ctx, PINK_STEP_SYSCALL);
	pink_context_set_options(ctx, PINK_TRACE_OPTION_EXEC);

	/* Create the event handler */
	handler = pink_event_handler_new();
	if (!handler) {
		fprintf(stderr, "pink_event_handler_new: %s\n", strerror(errno));
		return EXIT_FAILURE;
	}

	/* Tie the tracing context to the event handler */
	pink_event_handler_set_context(handler, ctx);

	/* Register the error handler */
	pink_event_handler_set_error_callback(handler, cb_error, NULL);

	/* Register the event handlers */
	pink_event_handler_set_ptrace_callback(handler, PINK_EVENT_SYSCALL, cb_syscall, NULL);
	pink_event_handler_set_ptrace_callback(handler, PINK_EVENT_EXEC, cb_exec, NULL);

	/* Enter trace loop */
	ret = pink_loop(handler, child_func, (void *)++argv);

	/* Cleanup and exit */
	pink_context_free(ctx);
	pink_event_handler_free(handler);
	return ret;
}
