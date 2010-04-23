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

#ifndef PINKTRACE_GUARD_CALLBACK_H
#define PINKTRACE_GUARD_CALLBACK_H 1

/**
 * \file
 * Pink's default callbacks
 **/

#include <stdbool.h>

#include <pinktrace/context.h>

/**
 * Default callback for signal events.
 * This callback resumes the child with either pink_trace_singlestep or
 * pink_trace_syscall() depending on the return value of
 * pink_context_get_step().
 *
 * \see PINK_EVENT_STOP
 * \see PINK_EVENT_GENUINE
 * \see PINK_EVENT_UNKNOWN
 *
 * \see pink_event_handler_set_signal_callback
 *
 * \param ctx The tracing context
 * \param pid Process ID of the child being traced
 * \param signum Last received signal
 * \param userdata The user data
 *
 * \return true on success, false on failure
 **/
bool
pink_callback_signal_default(const pink_context_t *ctx, pid_t pid, int signum, void *userdata);

/**
 * Default callback for system call event.
 * This callback resumes the child with either pink_trace_singlestep() or
 * pink_trace_syscall() depending on the return value of
 * pink_context_get_step().
 *
 * \see PINK_EVENT_SYSCALL
 *
 * \see pink_event_handler_set_ptrace_callback
 *
 * \param ctx The tracing context
 * \param pid Process ID of the child being traced
 * \param userdata The user data
 *
 * \return true on success, false on failure
 **/
bool
pink_callback_syscall_default(const pink_context_t *ctx, pid_t pid, void *userdata);

/**
 * Default callback for exit events.
 * If the given process ID is the eldest child, this callback exits with its
 * return status. In case it was terminated with a signal, this callback exits
 * with return code (128 + SIGNUM). Otherwise it does nothing and simply
 * returns.
 *
 * \see PINK_EVENT_EXIT_GENUINE
 * \see PINK_EVENT_EXIT_SIGNAL
 *
 * \see pink_event_handler_set_exit_callback
 *
 * \param ctx The tracing context
 * \param pid Process ID of the child being traced
 * \param excode Exit code or the terminating signal depending on the event
 * \param userdata The user data
 *
 * \return true on success, false on failure
 **/
bool
pink_callback_exit_default(const pink_context_t *ctx, pid_t pid, int excode, void *userdata);

#endif /* !PINKTRACE_GUARD_CALLBACK_H */
