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
#include <stdbool.h>
#include <stdlib.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

pink_event_handler_t *
pink_event_handler_new(void)
{
	pink_event_handler_t *handler;

	if ((handler = calloc(1, sizeof(pink_event_handler_t))) == NULL)
		return NULL;

	return handler;
}

void
pink_event_handler_free(pink_event_handler_t *handler)
{
	free(handler);
}

void
pink_event_handler_set_context(pink_event_handler_t *handler, pink_context_t *ctx)
{
	handler->ctx = ctx;
}

pink_context_t *
pink_event_handler_get_context(pink_event_handler_t *handler)
{
	return handler->ctx;
}

void
pink_event_handler_set_signal_callback(pink_event_handler_t *handler,
	pink_event_t event, pink_event_handler_signal_t callback,
	void *userdata)
{
	assert(event == PINK_EVENT_STOP || event == PINK_EVENT_GENUINE || event == PINK_EVENT_UNKNOWN);

	switch (event) {
	case PINK_EVENT_STOP:
		handler->cb_stop = callback;
		handler->userdata_stop = userdata;
		break;
	case PINK_EVENT_GENUINE:
		handler->cb_genuine = callback;
		handler->userdata_genuine = userdata;
		break;
	case PINK_EVENT_UNKNOWN:
		handler->cb_unknown = callback;
		handler->userdata_unknown = userdata;
		break;
	default:
		abort();
	}
}

pink_event_handler_signal_t
pink_event_handler_get_signal_callback(pink_event_handler_t *handler, pink_event_t event)
{
	assert(event == PINK_EVENT_STOP || event == PINK_EVENT_GENUINE || event == PINK_EVENT_UNKNOWN);

	switch (event) {
	case PINK_EVENT_STOP:
		return handler->cb_stop;
	case PINK_EVENT_GENUINE:
		return handler->cb_genuine;
	case PINK_EVENT_UNKNOWN:
		return handler->cb_unknown;
	default:
		abort();
	}
}

void
pink_event_handler_set_ptrace_callback(pink_event_handler_t *handler,
	pink_event_t event, pink_event_handler_ptrace_t callback,
	void *userdata)
{
	assert(event == PINK_EVENT_SYSCALL ||
		event == PINK_EVENT_FORK ||
		event == PINK_EVENT_VFORK ||
		event == PINK_EVENT_CLONE ||
		event == PINK_EVENT_VFORK_DONE ||
		event == PINK_EVENT_EXIT);

	switch (event) {
	case PINK_EVENT_SYSCALL:
		handler->cb_syscall = callback;
		handler->userdata_syscall = userdata;
		break;
	case PINK_EVENT_FORK:
		handler->cb_fork = callback;
		handler->userdata_fork = userdata;
		break;
	case PINK_EVENT_VFORK:
		handler->cb_vfork = callback;
		handler->userdata_vfork = userdata;
		break;
	case PINK_EVENT_CLONE:
		handler->cb_clone = callback;
		handler->userdata_clone = userdata;
		break;
	case PINK_EVENT_VFORK_DONE:
		handler->cb_vfork_done = callback;
		handler->userdata_vfork_done = userdata;
		break;
	case PINK_EVENT_EXIT:
		handler->cb_exit = callback;
		handler->userdata_exit = userdata;
		break;
	default:
		abort();
	}
}

pink_event_handler_ptrace_t
pink_event_handler_get_ptrace_callback(pink_event_handler_t *handler, pink_event_t event)
{
	assert(event == PINK_EVENT_SYSCALL ||
		event == PINK_EVENT_FORK ||
		event == PINK_EVENT_VFORK ||
		event == PINK_EVENT_CLONE ||
		event == PINK_EVENT_VFORK_DONE ||
		event == PINK_EVENT_EXIT);

	switch (event) {
	case PINK_EVENT_SYSCALL:
		return handler->cb_syscall;
	case PINK_EVENT_FORK:
		return handler->cb_fork;
	case PINK_EVENT_VFORK:
		return handler->cb_vfork;
	case PINK_EVENT_CLONE:
		return handler->cb_clone;
	case PINK_EVENT_VFORK_DONE:
		return handler->cb_vfork_done;
	case PINK_EVENT_EXIT:
		return handler->cb_exit;
	default:
		abort();
	}
}

void
pink_event_handler_set_exit_callback(pink_event_handler_t *handler,
	pink_event_t event, pink_event_handler_exit_t callback,
	void *userdata)
{
	assert(event == PINK_EVENT_EXIT_GENUINE || event == PINK_EVENT_EXIT_SIGNAL);

	switch (event) {
	case PINK_EVENT_EXIT_GENUINE:
		handler->cb_exit_genuine = callback;
		handler->userdata_exit_genuine = userdata;
		break;
	case PINK_EVENT_EXIT_SIGNAL:
		handler->cb_exit_signal = callback;
		handler->userdata_exit_signal = userdata;
		break;
	default:
		abort();
	}
}

pink_event_handler_exit_t
pink_event_handler_get_exit_callback(pink_event_handler_t *handler, pink_event_t event)
{
	assert(event == PINK_EVENT_EXIT_GENUINE || event == PINK_EVENT_EXIT_SIGNAL);

	switch (event) {
	case PINK_EVENT_EXIT_GENUINE:
		return handler->cb_exit_genuine;
	case PINK_EVENT_EXIT_SIGNAL:
		return handler->cb_exit_signal;
	default:
		abort();
	}
}

void
pink_event_handler_set_error_callback(pink_event_handler_t *handler,
	pink_event_handler_error_t callback,
	void *userdata)
{
	handler->cb_error = callback;
	handler->userdata_error = userdata;
}

pink_event_handler_error_t
pink_event_handler_get_error_callback(pink_event_handler_t *handler)
{
	return handler->cb_error;
}
