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

/** Table of callbacks **/
typedef struct {
	/** Callback for child birth aka process creation **/
	void (*birth) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *parent);

	/** Callback for child death aka process deletion **/
	void (*death) (struct pink_easy_context *ctx, pink_easy_process_t *current);

	/**
	 * The count of the process tree dropped to zero, or waitpid()
	 * returned -ECHILD; both of which mean tracing is done.
	 * If this callback is NULL, pink_easy_loop() will just return with
	 * success, which may not always be what you want.
	 **/
	void (*end) (struct pink_easy_context *ctx, bool echild);

	/** Errback for errors in the spawned child. **/
	int (*cerror) (pink_easy_child_error_t e);

	/** Errback for errors in the main process **/
	void (*error) (struct pink_easy_context *ctx, pink_easy_process_t *current);

	/** Callback for #PINK_EVENT_STOP **/
	short (*ev_stop) (struct pink_easy_context *ctx, pink_easy_process_t *current, bool suspended);

	/** Callback for #PINK_EVENT_SYSCALL **/
	short (*ev_syscall) (struct pink_easy_context *ctx, pink_easy_process_t *current, bool entering);

	/** Callback for #PINK_EVENT_FORK **/
	short (*ev_fork) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *child, bool alive);

	/** Callback for #PINK_EVENT_VFORK **/
	short (*ev_vfork) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *child, bool alive);

	/** Callback for #PINK_EVENT_CLONE **/
	short (*ev_clone) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *child, bool alive);

	/**
	 * Callback for #PINK_EVENT_EXEC
	 * Note, the bitness of current is updated before this callback is called.
	 **/
	short (*ev_exec) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_bitness_t old_bitness);

	/** Callback for #PINK_EVENT_EXIT **/
	short (*ev_exit) (struct pink_easy_context *ctx, pink_easy_process_t *current, unsigned long status);

	/** Callback for #PINK_EVENT_GENUINE **/
	short (*ev_genuine) (struct pink_easy_context *ctx, pink_easy_process_t *current, int stopsig);

	/** Callback for #PINK_EVENT_EXIT_GENUINE **/
	short (*ev_exit_genuine) (struct pink_easy_context *ctx, pink_easy_process_t *current, int code);

	/** Callback for #PINK_EVENT_EXIT_SIGNAL **/
	short (*ev_exit_signal) (struct pink_easy_context *ctx, pink_easy_process_t *current, int sig);
} pink_easy_callback_t;

#endif /* !PINKTRACE_EASY_GUARD_CALLBACK_H */
