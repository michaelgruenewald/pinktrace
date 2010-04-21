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

#ifndef PINKTRACE_GUARD_CHECK_PINKTRACE_H
#define PINKTRACE_GUARD_CHECK_PINKTRACE_H 1

#include <check.h>

Suite *
bitness_suite_create(void);

Suite *
context_suite_create(void);

Suite *
fork_suite_create(void);

Suite *
event_suite_create(void);

Suite *
trace_suite_create(void);

#endif /* !PINKTRACE_GUARD_CHECK_PINKTRACE_H */
