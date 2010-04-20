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
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <pinktrace/context.h>
#include <pinktrace/fork.h>
#include <pinktrace/trace.h>

pid_t
pink_fork(pink_context_t *ctx)
{
	int status;
	pid_t pid;

	assert(ctx != NULL);

	if ((pid = fork()) < 0)
		return PINK_FORK_ERROR_FORK;
	else if (!pid) { /* child */
		if (!pink_trace_me())
			_exit(-1);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		if (WIFEXITED(pid))
			return PINK_FORK_ERROR_TRACE;

		if (!pink_trace_setup(pid, pink_context_get_options(ctx))) {
			/* Setting up child failed, kill it with fire! */
			kill(pid, SIGKILL);
			return PINK_FORK_ERROR_SETUP;
		}

		pink_context_set_eldest(ctx, pid);
	}
	return pid;
}

pid_t
pink_vfork(pink_context_t *ctx)
{
	int status;
	pid_t pid;

	assert(ctx != NULL);

	if ((pid = vfork()) < 0)
		return PINK_FORK_ERROR_FORK;
	else if (!pid) { /* child */
		if (!pink_trace_me())
			_exit(-1);
	}
	else { /* parent */
		waitpid(pid, &status, 0);

		if (WIFEXITED(pid))
			return PINK_FORK_ERROR_TRACE;

		if (!pink_trace_setup(pid, pink_context_get_options(ctx))) {
			/* Setting up child failed, kill it with fire! */
			kill(pid, SIGKILL);
			return PINK_FORK_ERROR_SETUP;
		}

		pink_context_set_eldest(ctx, pid);
	}
	return pid;
}
