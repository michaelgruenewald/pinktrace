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

#ifndef PINKTRACE_GUARD_HANDLER_H
#define PINKTRACE_GUARD_HANDLER_H 1

/**
 * \file
 * Pink's high level event handler
 **/

#include <stdbool.h>
#include <sys/types.h>

#include <pinktrace/context.h>
#include <pinktrace/event.h>

/**
 * This type definition represents the return value of the error handler
 * function.
 **/
typedef struct {
	/** Whether the error is fatal **/
	bool fatal;
	/** If the error is fatal, return code from the loop **/
	int code;
} pink_error_return_t;

/**
 * This opaque structure represents an event handler.
 **/
typedef struct pink_event_handler pink_event_handler_t;

/**
 * This function definition represents a signal event callback.
 * Corresponds to the events:
 *  - PINK_EVENT_STOP
 *  - PINK_EVENT_GENUINE
 *  - PINK_EVENT_UNKNOWN
 **/
typedef bool (*pink_event_handler_signal_t) (const pink_context_t *ctx,
	pid_t pid, int signum, void *userdata);

/**
 * This function definition represents a ptrace event callback.
 * Corresponds to the events:
 *  - PINK_EVENT_SYSCALL
 *  - PINK_EVENT_FORK
 *  - PINK_EVENT_VFORK
 *  - PINK_EVENT_CLONE
 *  - PINK_EVENT_VFORK_DONE
 *  - PINK_EVENT_EXIT
 **/
typedef bool (*pink_event_handler_ptrace_t) (const pink_context_t *ctx,
	pid_t pid, void *userdata);

/**
 * This function definition represents an exit event callback.
 * Corresponds to the events:
 *  - PINK_EVENT_EXIT_GENUINE
 *  - PINK_EVENT_EXIT_SIGNAL
 * For the former event the second argument is the exit code, for the latter
 * it's the terminating signal.
 **/
typedef bool (*pink_event_handler_exit_t) (const pink_context_t *ctx,
	pid_t pid, int excode, void *userdata);

/**
 * This function definition represents the error handler.
 **/
typedef pink_error_return_t (*pink_event_handler_error_t) (pink_event_handler_t *handler,
	pid_t pid, void *userdata);

/**
 * Allocate a new event handler
 *
 * \return An event handler on success, NULL on failure and sets errno
 * accordingly.
 **/
pink_event_handler_t *
pink_event_handler_new(void);

/**
 * Free the given event handler
 *
 * \param handler The event handler to be freed.
 **/
void
pink_event_handler_free(pink_event_handler_t *handler);

/**
 * Set the tracing context for the event handler
 *
 * \param handler The event handler
 * \param ctx The tracing context which is to be attached to the event handler.
 **/
void
pink_event_handler_set_context(pink_event_handler_t *handler, pink_context_t *ctx);

/**
 * Accessor function for the tracing context
 *
 * \param handler The event handler
 *
 * \return The tracing context of the event handler.
 **/
pink_context_t *
pink_event_handler_get_context(pink_event_handler_t *handler);

/**
 * Set signal event callbacks
 *
 * \param handler The event handler
 * \param event One of:
 *  - PINK_EVENT_STOP
 *  - PINK_EVENT_GENUINE
 *  - PINK_EVENT_UNKNOWN
 * \param callback The event handler callback
 * \param userdata The user data to be passed to the callback
 **/
void
pink_event_handler_set_signal_callback(pink_event_handler_t *handler,
	pink_event_t event, pink_event_handler_signal_t callback,
	void *userdata);

/**
 * Accessor function for the signal event callbacks.
 *
 * \param handler The event handler
 * \param event One of:
 *  - PINK_EVENT_STOP
 *  - PINK_EVENT_GENUINE
 *  - PINK_EVENT_UNKNOWN
 *
 * \return The signal event callback of the given event
 **/
pink_event_handler_signal_t
pink_event_handler_get_signal_callback(pink_event_handler_t *handler, pink_event_t event);

/**
 * Set ptrace event callbacks
 *
 * \param handler The event handler
 * \param event One of:
 *  - PINK_EVENT_SYSCALL
 *  - PINK_EVENT_FORK
 *  - PINK_EVENT_VFORK
 *  - PINK_EVENT_CLONE
 *  - PINK_EVENT_VFORK_DONE
 *  - PINK_EVENT_EXIT
 * \param callback The event handler callback
 * \param userdata The user data to be passed to the callback
 **/
void
pink_event_handler_set_ptrace_callback(pink_event_handler_t *handler,
	pink_event_t event, pink_event_handler_ptrace_t callback,
	void *userdata);

/**
 * Accessor function for the ptrace event callbacks
 *
 * \param handler The event handler
 * \param event One of:
 *  - PINK_EVENT_SYSCALL
 *  - PINK_EVENT_FORK
 *  - PINK_EVENT_VFORK
 *  - PINK_EVENT_CLONE
 *  - PINK_EVENT_VFORK_DONE
 *  - PINK_EVENT_EXIT
 *
 * \return The ptrace event callback of the given event
 **/
pink_event_handler_ptrace_t
pink_event_handler_get_ptrace_callback(pink_event_handler_t *handler, pink_event_t event);

/**
 * Set exit event callbacks
 *
 * \param handler The event handler
 * \param event One of:
 *  - PINK_EVENT_EXIT_GENUINE
 *  - PINK_EVENT_EXIT_SIGNAL
 * \param callback The exit event handler callback
 * \param userdata The user data to be passed to the callback
 **/
void
pink_event_handler_set_exit_callback(pink_event_handler_t *handler,
	pink_event_t event, pink_event_handler_exit_t callback,
	void *userdata);

/**
 * Accessor function for the exit event callbacks
 *
 * \param handler The event handler
 * \param event One of:
 *  - PINK_EVENT_EXIT_GENUINE
 *  - PINK_EVENT_EXIT_SIGNAL
 *
 * \return The exit event callback of the given event
 **/
pink_event_handler_exit_t
pink_event_handler_get_exit_callback(pink_event_handler_t *handler, pink_event_t event);

/**
 * Set the error handler of the event handler
 *
 * \param handler The event handler
 * \param callback The error handler callback
 * \param userdata The user data to be passed to the function
 **/
void
pink_event_handler_set_error_callback(pink_event_handler_t *handler,
	pink_event_handler_error_t callback,
	void *userdata);

/**
 * Accessor function for the error handler
 *
 * \param handler The event handler
 *
 * \return The error handler callback
 **/
pink_event_handler_error_t
pink_event_handler_get_error_callback(pink_event_handler_t *handler);

#endif /* !PINKTRACE_GUARD_HANDLER_H */
