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

#ifndef PINKTRACE_GUARD_GCC_H
#define PINKTRACE_GUARD_GCC_H 1

/**
 * \file
 * Pink's GCC macros
 **/

#if !defined(SPARSE) && defined(__GNUC__) && __GNUC__ >= 3

#define pink_noreturn __attribute__((noreturn))
#define pink_unused __attribute__((unused))
#define pink_likely(x) __builtin_expect(!!(x), 1)
#define pink_unlikely(x) __builtin_expect(!!(x), 0)

#else
/**
 * GCC noreturn attribute
 **/
#define pink_noreturn
/**
 * GCC unused attribute
 **/
#define pink_unused
/**
 * GCC builtin_expect macro
 **/
#define pink_likely(x) (x)
/**
 * GCC builtin_expect macro
 **/
#define pink_unlikely(x) (x)

#endif /* !defined(SPARSE) ... */

#endif /* !PINKTRACE_GUARD_GCC_H */
