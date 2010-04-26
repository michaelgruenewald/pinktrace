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

#ifndef PINKTRACE_GUARD_EVENT_H
#define PINKTRACE_GUARD_EVENT_H 1

/**
 * \file
 * Pink's event handling
 **/

#include <stdbool.h>

/**
 * Event constants
 **/
typedef enum {
	/** Child has been stopped **/
	PINK_EVENT_STOP,
	/** Child has entered/exited a system call **/
	PINK_EVENT_SYSCALL,
	/** Child has called fork() **/
	PINK_EVENT_FORK,
	/** Child has called vfork() **/
	PINK_EVENT_VFORK,
	/** Child has called clone() **/
	PINK_EVENT_CLONE,
	/** Child has exited a vfork() call **/
	PINK_EVENT_VFORK_DONE,
	/** Child has called execve() **/
	PINK_EVENT_EXEC,
	/** Child is exiting (ptrace way, stopped before exit) **/
	PINK_EVENT_EXIT,
	/** Child has received a genuine signal **/
	PINK_EVENT_GENUINE,
	/** Child has exited normally **/
	PINK_EVENT_EXIT_GENUINE,
	/** Child has been terminated with a signal **/
	PINK_EVENT_EXIT_SIGNAL,
	/** Unknown event, shouldn't happen **/
	PINK_EVENT_UNKNOWN,
} pink_event_t;

/**
 * Return the last event made by child.
 *
 * \bug This function doesn't work if sysgood is set to false.
 *
 * \param status The status argument, received from wait(2) system call.
 * \param sysgood Set this to true if you passed #PINK_TRACE_OPTION_SYSGOOD to
 * pink_trace_setup().
 *
 * \return One of PINK_EVENT_* constants
 **/
pink_event_t
pink_event_decide(int status, bool sysgood);

/**
 * Return a string representation of the event.
 *
 * \param event The event to be stringified.
 *
 * \return The string representation of the event.
 **/
const char *
pink_event_tostring(pink_event_t event);

#endif /* !PINKTRACE_GUARD_EVENT_H */
