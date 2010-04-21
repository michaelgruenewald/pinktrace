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

#ifndef PINKTRACE_GUARD_ERROR_H
#define PINKTRACE_GUARD_ERROR_H 1

/**
 * \file
 * Pink's event handling
 **/

#include <pinktrace/context.h>

enum pink_event {
	PINK_EVENT_STOP,	/*<< Child has been stopped */
	PINK_EVENT_SYSCALL,	/*<< Child has entered/exited a system call */
	PINK_EVENT_FORK,	/*<< Child called fork() */
	PINK_EVENT_VFORK,	/*<< Child called vfork() */
	PINK_EVENT_CLONE,	/*<< Child called clone() */
	PINK_EVENT_VFORK_DONE,	/*<< Child is exiting a vfork() call */
	PINK_EVENT_EXEC,	/*<< Child called execve() */
	PINK_EVENT_EXIT,	/*<< Child is exiting (ptrace way, stopped before exit) */
	PINK_EVENT_GENUINE,	/*<< Child has received a genuine signal */
	PINK_EVENT_EXIT_GENUINE,/*<< Child has *exited* normally */
	PINK_EVENT_EXIT_SIGNAL, /*<< Child exited with a signal */
	PINK_EVENT_UNKNOWN,	/*<< Unknown event, shouldn't happen */
};

/**
 * Return the last event made by child.
 *
 * \param ctx The tracing context
 * \param status The status argument, received from wait(2) system call.
 *
 * \return One of PINK_EVENT_* constants
 **/
unsigned int
pink_event(pink_context_t *ctx, int status);

#endif /* !PINKTRACE_GUARD_EVENT_H */
