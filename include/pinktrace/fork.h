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

#ifndef PINKTRACE_GUARD_FORK_H
#define PINKTRACE_GUARD_FORK_H 1

/**
 * \file
 * Pink's fork wrapper
 **/

#include <sys/types.h>

#include <pinktrace/context.h>

/**
 * fork(2) wrapper that sets up the child for tracing.
 *
 * \see pink_error_t
 *
 * \param ctx The tracing context to be used.
 *
 * \return On success, the process ID of the child process is returned in the
 * parent, and 0 is returned in the child and the eldest child property of the
 * context is updated. The child stops itself with a SIGSTOP and needs to be
 * resumed. On failure, one of PINK_ERROR_* constants is returned, the child is
 * either never created or killed and errno is set accordingly.
 **/
pid_t
pink_fork(pink_context_t *ctx);

#endif /* !PINKTRACE_GUARD_FORK_H */
