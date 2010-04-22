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

#ifndef PINKTRACE_GUARD_LOOP_H
#define PINKTRACE_GUARD_LOOP_H 1

#include <pinktrace/handler.h>

/**
 * This function definition represents the only function to be called from the
 * traced child.
 **/
typedef int (*pink_child_func_t) (void *userdata);

/**
 * Enter the tracing loop and use event handlers as callbacks.
 * This function sets tracing options depending on the registered event
 * handlers so the caller doesn't necessarily have to set tracing options
 * beforehand.
 *
 * \param handler The event handler to govern the loop.
 * \param func The function to be called from child, unused if the child is
 * being attached to.
 * \param userdata The user data to be passed to the function.
 *
 * \return The exit code of the eldest child.
 **/
int
pink_loop(pink_event_handler_t *handler, pink_child_func_t func, void *userdata);

#endif /* !PINKTRACE_GUARD_LOOP_H */
