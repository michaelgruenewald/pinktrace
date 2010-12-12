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

/** Valid return values from #pink_easy_errback_main_t **/
typedef enum {
	/** Abort the event loop, return the last error negated **/
	PINK_EASY_ERRBACK_ABORT = 0,
	/** Ignore the error and continue
	 * \warning May be dangerous in some cases, use with care!
	 **/
	PINK_EASY_ERRBACK_IGNORE,
	/** Remove the child from the process tree and continue tracing. **/
	PINK_EASY_ERRBACK_KILL,
} pink_easy_errback_return_t;

/** Abort the event loop **/
#define PINK_EASY_CALLBACK_ABORT  00001

struct pink_easy_context;

/**
 * Errback for errors in the main process.
 * - Use pink_easy_context_get_error() to get the error code.
 * - Use pink_easy_context_get_error_fatal() to get whether error is fatal.
 *
 * There are a few important points about this callback:
 * - The variable arguments give extra information about the error condition
 *   and they vary between different error conditions.
 * - After some error conditions, the global variable errno may also give
 *   information about the failure reason of the underlying library call.
 * - If the error is not fatal the return value of this callback specifies the
 *   decision.
 *
 * Here's a list of possible error conditions
 *
 * \verbatim
     -------------------------------------------------------------------------------
     - Error             fatal?   errno  Arguments                                 -
     -------------------------------------------------------------------------------
     - ALLOC_ELDEST      true     +      -                                         -
     - ALLOC_PREMATURE   true     +      pid_t pid                                 -
     - ALLOC_FORK        true     +      pink_easy_process_t *current, pid_t cpid  -
     - FORK              true     +      -                                         -
     - VFORK             true     +      -                                         -
     - WAIT_ELDEST       true     +      pid_t pid                                 -
     - SIGNAL_ELDEST     true     -      pid_t pid, int status                     -
     - SETUP_ELDEST      true     +      pid_t pid                                 -
     - BITNESS_ELDEST    false    +      pid_t pid                                 -
     - BITNESS_PREMATURE false    +      pid_t pid                                 -
     - BITNESS           false    +      pink_easy_process_t *current              -
     - STEP_INITIAL      true     +      pink_easy_process_t *current              -
     - STEP_STOP         false    +      pink_easy_process_t *current              -
     - STEP_SYSCALL      false    +      pink_easy_process_t *current              -
     - STEP_PREMATURE    false    +      pink_easy_process_t *current              -
     - STEP_FORK         false    +      pink_easy_process_t *current              -
     - STEP_EXEC         false    +      pink_easy_process_t *current              -
     - STEP_EXIT         false    +      pink_easy_process_t *current              -
     - STEP_GENUINE      false    +      pink_easy_process_t *current, int status  -
     - WAIT              true     +      pink_easy_process_t *current              -
     - WAIT_ALL          true     +      -                                         -
     - GETEVENTMSG_FORK  false    +      pink_easy_process_t *current              -
     - GETEVENTMSG_EXIT  false    +      pink_easy_process_t *current              -
     - EVENT_UNKNOWN     false    -      pink_easy_process_t *current, int status  -
     -------------------------------------------------------------------------------
   \endverbatim
 *
 * \param ctx Tracing context
 * \param ... Variable arguments give extra information about the error.
 *
 * \return Decision if error is not fatal, ignored otherwise.
 **/
typedef pink_easy_errback_return_t (*pink_easy_errback_main_t) (const struct pink_easy_context *ctx, ...);

/**
 * Errback for errors in the spawned child.
 *
 * \param e Error code
 *
 * \return Child exists with this return value
 **/
typedef int (*pink_easy_errback_child_t) (pink_easy_child_error_t e);

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

/** This structure represents a callback table **/
typedef struct pink_easy_callback_table {
	/** "main" errback **/
	pink_easy_errback_main_t eb_main;
	/** "child" errback **/
	pink_easy_errback_child_t eb_child;

	/** "birth" callback **/
	pink_easy_callback_birth_t cb_birth;
	/** "death" callback **/
	pink_easy_callback_death_t cb_death;
	/** "end" callback **/
	pink_easy_callback_end_t cb_end;

	/** "event_stop" callback **/
	pink_easy_callback_event_stop_t cb_event_stop;
	/** "event_syscall" callback **/
	pink_easy_callback_event_syscall_t cb_event_syscall;
	/** "event_fork" callback **/
	pink_easy_callback_event_fork_t cb_event_fork;
	/** "event_vfork" callback **/
	pink_easy_callback_event_fork_t cb_event_vfork;
	/** "event_clone" callback **/
	pink_easy_callback_event_fork_t cb_event_clone;
	/** "event_exec" callback **/
	pink_easy_callback_event_exec_t cb_event_exec;
	/** "event_exit" callback **/
	pink_easy_callback_event_exit_t cb_event_exit;
	/** "event_genuine" callback **/
	pink_easy_callback_event_genuine_t cb_event_genuine;

	/** "exit" callback **/
	pink_easy_callback_exit_t cb_exit;
	/** "exit_signal" callback **/
	pink_easy_callback_exit_t cb_exit_signal;
} pink_easy_callback_table_t;

#endif /* !PINKTRACE_EASY_GUARD_CALLBACK_H */
