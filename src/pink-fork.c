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

#include <assert.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <pinktrace/internal.h>
#include <pinktrace/context.h>
#include <pinktrace/error.h>
#include <pinktrace/fork.h>
#include <pinktrace/trace.h>

pid_t
pink_fork(pink_context_t *ctx)
{
	int save_errno, status;
	pid_t pid;

	assert(ctx != NULL);

	if ((pid = fork()) < 0) {
		ctx->error = PINK_ERROR_FORK;
		return -1;
	}
	else if (!pid) { /* child */
		if (!pink_trace_me())
			_exit(-1);
		kill(getpid(), SIGSTOP);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		if (WIFEXITED(pid)) {
			ctx->error = PINK_ERROR_TRACE;
			return -1;
		}

		assert(WIFSTOPPED(status));
		assert(WSTOPSIG(status) == SIGSTOP);

		if (!pink_trace_setup(pid, ctx->options)) {
			/* Setting up child failed, kill it with fire! */
			save_errno = errno;
			kill(pid, SIGKILL);
			errno = save_errno;

			ctx->error = PINK_ERROR_TRACE_SETUP;
			return -1;
		}

		ctx->eldest = pid;
	}
	return pid;
}
