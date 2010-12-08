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

#ifndef PINKTRACE_EASY_GUARD_CALLBACK_H
#define PINKTRACE_EASY_GUARD_CALLBACK_H 1

#include <stdbool.h>

#include <pinktrace/easy/error.h>
#include <pinktrace/easy/process.h>

/**
 * \file
 * Pink's easy ptrace() event callbacks
 **/

/** Abort the event loop **/
#define PINK_EASY_CALLBACK_ABORT 00001

struct pink_easy_context;

/**
 * Callback for child birth.
 *
 * This is called when a new process is created.
 *
 * \param ctx Tracing context
 * \param current New born child
 * \param parent The parent of the new child or NULL for the eldest child.
 **/
typedef void (*pink_easy_callback_birth_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *parent);

/**
 * Callback for child death.
 *
 * This is called after a process exited and removed from the process tree.
 *
 * \param ctx Tracing context
 * \param current Dead child
 **/
typedef void (*pink_easy_callback_death_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current);

/**
 * Callback for the end of tracing.
 *
 * This is called when the count of the process tree dropped to zero, or
 * waitpid() returned -ECHILD. If this callback is NULL, pink_easy_loop() will
 * just return with success, which may not always be what you want.
 *
 * \see pink_easy_loop
 *
 * \param ctx Tracing context
 * \param echild true if waitpid() failed with -ECHILD, false if process tree
 * dropped to zero.
 *
 * \return This value is returned by pink_easy_loop().
 **/
typedef int (*pink_easy_callback_end_t) (const struct pink_easy_context *ctx, bool echild);

/**
 * Callback for errors in the main process.
 * Use pink_easy_context_get_error() to get the error code.
 *
 * \param ctx Tracing context
 * \param current Current child, may be NULL
 **/
typedef void (*pink_easy_callback_error_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current);

/**
 * Callback for errors in the spawned child.
 *
 * \param e Error code
 *
 * \return Child exists with this return value
 **/
typedef int (*pink_easy_callback_cerror_t) (pink_easy_child_error_t e);

/**
 * Callback for #PINK_EVENT_STOP
 *
 * \param ctx Tracing context
 * \param current Current child
 * \param suspended true if the child is suspended. This may happen when the
 * child is born before we receive a #PINK_EVENT_FORK. In this case the process
 * is added to the process tree but not resumed until #PINK_EVENT_FORK is
 * received.
 *
 * \return Bitwise OR'ed PINK_EASY_CALLBACK_* flags
 **/
typedef short (*pink_easy_callback_event_stop_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, bool suspended);

/**
 * Callback for #PINK_EVENT_SYSCALL
 *
 * \param ctx Tracing context
 * \param current Current child
 * \param entering true if the child is entering the system call, false
 * otherwise.
 *
 * \return Bitwise OR'ed PINK_EASY_CALLBACK_* flags
 **/
typedef short (*pink_easy_callback_event_syscall_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, bool entering);

/**
 * Callback for #PINK_EVENT_FORK, #PINK_EVENT_VFORK and #PINK_EVENT_CLONE
 *
 * \param ctx Tracing context
 * \param current Current child
 * \param alive true if the child was born and suspended before, false
 * otherwise.
 *
 * \return Bitwise OR'ed PINK_EASY_CALLBACK_* flags
 **/
typedef short (*pink_easy_callback_event_fork_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *child, bool alive);

/**
 * Callback for #PINK_EVENT_EXEC
 *
 * \note The bitness of the child is updated before this callback is called.
 *
 * \param ctx Tracing context
 * \param current Current child
 * \param old_bitness Old bitness of the child
 *
 * \return Bitwise OR'ed PINK_EASY_CALLBACK_* flags
 **/
typedef short (*pink_easy_callback_event_exec_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, pink_bitness_t old_bitness);

/**
 * Callback for #PINK_EVENT_EXIT
 *
 * \param ctx Tracing context
 * \param current Current child
 * \param status Exit status
 *
 * \return Bitwise OR'ed PINK_EASY_CALLBACK_* flags
 **/
typedef short (*pink_easy_callback_event_exit_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, unsigned long status);

/**
 * Callback for #PINK_EVENT_GENUINE
 *
 * \param ctx Tracing context
 * \param current Current process
 * \param stopsig Stop signal
 *
 * \return Bitwise OR'ed PINK_EASY_CALLBACK_* flags
 **/
typedef short (*pink_easy_callback_event_genuine_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, int stopsig);

/**
 * Callback for #PINK_EVENT_EXIT and #PINK_EVENT_EXIT_SIGNAL
 *
 * \param ctx Tracing context
 * \param current Current child
 * \param cs Exit code for #PINK_EVENT_EXIT, termination signal for
 * #PINK_EVENT_SIGNAL.
 *
 * \return Bitwise OR'ed PINK_EASY_CALLBACK_* flags
 **/
typedef short (*pink_easy_callback_exit_t) (const struct pink_easy_context *ctx, pink_easy_process_t *current, int cs);

#endif /* !PINKTRACE_EASY_GUARD_CALLBACK_H */
