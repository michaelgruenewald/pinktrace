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

#ifndef PINKTRACE_EASY_GUARD_CONTEXT_H
#define PINKTRACE_EASY_GUARD_CONTEXT_H 1

#include <pinktrace/pink.h>
#include <pinktrace/easy/callback.h>
#include <pinktrace/easy/error.h>
#include <pinktrace/easy/func.h>
#include <pinktrace/easy/process.h>

/**
 * \file
 * Pink's easy tracing context
 **/

/** Opaque structure which represents a tracing context. **/
typedef struct pink_easy_context pink_easy_context_t;

/**
 * Allocate a tracing context.
 *
 * \note This function accepts a destructor function pointer which may be used
 * to free the user data. You may pass NULL if you want to handle the
 * destruction yourself or use the standard free() function from stdlib.h for
 * basic destruction.
 *
 * \param options Options for pink_trace_setup()
 * \param ctable Callback table
 * \param data User data
 * \param func The desctructor function for the user data
 *
 * \return The tracing context on success, NULL on failure and sets errno
 * accordingly.
 **/
PINK_NONNULL(2)
pink_easy_context_t *
pink_easy_context_new(int options, const pink_easy_callback_t *cb, void *data, pink_easy_free_func_t func);

/**
 * Destroy a tracing context; destroys the process tree and all the members of
 * the tree. The user data of the processes are free'd if a destructor function
 * was provided with pink_easy_process_set_data(). The user data of the context
 * is free'd if a destructor function was provided with
 * pink_easy_context_new(). If you need to do further cleanup, use the
 * cb_destroy callback provided by #pink_easy_callback_t.
 *
 * \param ctx Tracing context
 **/
PINK_NONNULL(1)
void
pink_easy_context_destroy(pink_easy_context_t *ctx);

/**
 * Returns the last error saved in the context.
 *
 * \param ctx Tracing context
 *
 * \return Error condition
 **/
PINK_NONNULL(1)
pink_easy_error_t
pink_easy_context_get_error(const pink_easy_context_t *ctx);

/**
 * Clears the error saved in the context.
 *
 * \param ctx Tracing context
 **/
PINK_NONNULL(1)
void
pink_easy_context_clear_error(pink_easy_context_t *ctx);

/**
 * Returns the eldest process in the tree. The eldest process is quite often
 * useful when tracing (e.g. when determining the exit code) so pinktrace keeps
 * a reference to this process entry.
 *
 * \param ctx Tracing context
 *
 * \return The eldest process
 **/
PINK_NONNULL(1)
pink_easy_process_t *
pink_easy_context_get_eldest(const pink_easy_context_t *ctx);

/**
 * Returns the user data of the tracing context
 *
 * \param ctx Tracing context
 *
 * \return User data
 **/
PINK_NONNULL(1)
void *
pink_easy_context_get_data(const pink_easy_context_t *ctx);

/**
 * Returns the process tree
 *
 * \param ctx Tracing context
 *
 * \return Process tree
 **/
PINK_NONNULL(1)
pink_easy_process_tree_t *
pink_easy_context_get_tree(const pink_easy_context_t *ctx);

#endif /* !PINKTRACE_EASY_GUARD_CONTEXT_H */
