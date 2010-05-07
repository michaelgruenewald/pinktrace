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

#ifndef PINKTRACE_GUARD_PINK_H
#define PINKTRACE_GUARD_PINK_H 1

/**
 * \mainpage pinktrace
 *
 * pinktrace - Pink's Tracing Library
 *
 * pinktrace is a wrapper around ptrace(2) system call.
 * It provides a robust API for tracing processes.
 *
 * Note this is a work in progress and the API is *not* stable.
 *
 * \author Ali Polatel <alip@exherbo.org>
 **/

/**
 * \example pink-about.c
 * This is a simple example demonstrating how to use Pink's version macros.
 **/

/**
 * \example pink-fork-freebsd.c
 * This is an example demonstrating how to fork and start tracing a process on FreeBSD.
 **/

/**
 * \example pink-fork-linux.c
 * This is an example demonstrating how to fork and start tracing a process on Linux.
 **/

/**
 * \example pink-simple-strace-freebsd.c
 * This is a simple strace like program example written with pinktrace for FreeBSD.
 **/

/**
 * \example pink-simple-strace-linux.c
 * This is a simple strace like program example written with pinktrace for Linux.
 **/

/**
 * \file
 * A header file including all other header files part of pinktrace
 **/

#include <pinktrace/gcc.h>
#include <pinktrace/about.h>
#include <pinktrace/bitness.h>
#include <pinktrace/decode.h>
#include <pinktrace/encode.h>
#include <pinktrace/event.h>
#include <pinktrace/name.h>
#include <pinktrace/socket.h>
#include <pinktrace/trace.h>
#include <pinktrace/util.h>

#endif /* !PINKTRACE_GUARD_PINK_H */
