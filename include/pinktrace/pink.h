/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef PINKTRACE_GUARD_PINK_H
#define PINKTRACE_GUARD_PINK_H 1

/**
 * \mainpage pinktrace
 *
 * pinktrace - Pink's Tracing Library
 *
 * \section overview Overview
 *
 * pinktrace is a wrapper around \c ptrace(2) system call.
 * It provides a robust API for tracing processes.
 *
 * \attention This is a work in progress and the API is \b not stable.
 *
 * \section parts Parts
 *
 * This library is divided into two parts:
 * - \ref core
 * - \ref easy
 *
 * \author Ali Polatel <alip@exherbo.org>
 **/

/**
 * \defgroup core Core library (pinktrace)
 *
 * This is a low-level library implementing thin wrappers around \c ptrace(2)
 * system call.
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
 * \brief A header file including all other header files part of pinktrace
 **/

#include <pinktrace/compat.h>
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
