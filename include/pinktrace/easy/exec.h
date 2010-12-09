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
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LpIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef PINKTRACE_GUARD_EASY_EXEC_H
#define PINKTRACE_GUARD_EASY_EXEC_H 1

#include <pinktrace/easy/context.h>

/**
 * \file
 * Pink's easy execve() wrappers
 **/

/**
 * This function calls vfork() to spawn a new child, does the necessary
 * preparation for tracing and then calls execve().
 *
 * \param ctx Tracing context
 * \param filename Path of the executable
 * \param argv Arguments
 * \param envp An array of strings, which are passed as environment to the new
 * program
 *
 * \return Depends on the callbacks, see pink_easy_loop()
 **/
PINK_NONNULL(1)
int
pink_easy_execve(pink_easy_context_t *ctx, const char *filename, char *const argv[], char *const envp[]);

/**
 * This function calls vfork() to spawn a new child, does the necessary
 * preparation for tracing, handles the arguments and calls execve().
 *
 * \param ctx Tracing context
 * \param file Filename of the executable
 * \param arg Initial argument, filename of the executable
 * \param ... Variable arguments which must be terminated by a NULL pointer
 *
 * \return Depends on the callbacks, see pink_easy_loop()
 **/
PINK_NONNULL(1)
int
pink_easy_execl(pink_easy_context_t *ctx, const char *file, const char *arg, ...);

/**
 * This function calls vfork() to spawn a new child, does the necessary
 * preparation for tracing, handles the arguments and calls execvp().
 *
 * \param ctx Tracing context
 * \param file Filename of the executable
 * \param arg Initial argument, filename of the executable
 * \param ... Variable arguments which must be terminated by a NULL pointer
 *
 * \return Depends on the callbacks, see pink_easy_loop()
 **/
PINK_NONNULL(1)
int
pink_easy_execlp(pink_easy_context_t *ctx, const char *file, const char *arg, ...);

/**
 * This function calls vfork() to spawn a new child, does the necessary
 * preparation for tracing and then calls execv().
 *
 * \param ctx Tracing context
 * \param path Path of the executable
 * \param argv Arguments
 *
 * \return Depends on the callbacks, see pink_easy_loop()
 **/
PINK_NONNULL(1)
int
pink_easy_execv(pink_easy_context_t *ctx, const char *path, char *const argv[]);

/**
 * This function calls vfork() to spawn a new child, does the necessary
 * preparation for tracing and then calls execvp().
 *
 * \param ctx Tracing context
 * \param file Name of the executable
 * \param argv Arguments
 *
 * \return Depends on the callbacks, see pink_easy_loop()
 **/
PINK_NONNULL(1)
int
pink_easy_execvp(pink_easy_context_t *ctx, const char *file, char *const argv[]);

#endif /* !PINKTRACE_GUARD_EASY_EXEC_H */
