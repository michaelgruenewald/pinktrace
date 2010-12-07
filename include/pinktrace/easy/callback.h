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
	/** Callback called when the tracing context is destroyed. **/
	void (*cb_destroy) (void *data);

	/** Callback for child birth aka process creation **/
	void (*cb_birth) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *parent);

	/** Callback for child death aka process deletion **/
	void (*cb_death) (struct pink_easy_context *ctx, pink_easy_process_t *current);

	/** Errback for errors in the spawned child. **/
	int (*eb_child) (pink_easy_cerror_t e);

	/** Errback for errors in the main process **/
	void (*eb_main) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_error_t e);

	/** Callback for #PINK_EVENT_STOP **/
	short (*cb_stop) (struct pink_easy_context *ctx, pink_easy_process_t *current, bool suspended);

	/** Callback for #PINK_EVENT_SYSCALL **/
	short (*cb_syscall) (struct pink_easy_context *ctx, pink_easy_process_t *current, bool entering);

	/** Callback for #PINK_EVENT_FORK **/
	short (*cb_fork) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *child, bool alive);

	/** Callback for #PINK_EVENT_VFORK **/
	short (*cb_vfork) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *child, bool alive);

	/** Callback for #PINK_EVENT_CLONE **/
	short (*cb_clone) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_easy_process_t *child, bool alive);

	/**
	 * Callback for #PINK_EVENT_EXEC
	 * Note, the bitness of current is updated before this callback is called.
	 **/
	short (*cb_exec) (struct pink_easy_context *ctx, pink_easy_process_t *current, pink_bitness_t orig_bitness);

	/** Callback for #PINK_EVENT_EXIT **/
	short (*cb_exit) (struct pink_easy_context *ctx, pink_easy_process_t *current, unsigned long status);

	/** Callback for #PINK_EVENT_GENUINE **/
	short (*cb_genuine) (struct pink_easy_context *ctx, pink_easy_process_t *current, int stopsig);

	/** Callback for #PINK_EVENT_EXIT_GENUINE **/
	short (*cb_exit_genuine) (struct pink_easy_context *ctx, pink_easy_process_t *current, int code);

	/** Callback for #PINK_EVENT_EXIT_SIGNAL **/
	short (*cb_exit_signal) (struct pink_easy_context *ctx, pink_easy_process_t *current, int sig);
} pink_easy_callback_t;

#endif /* !PINKTRACE_EASY_GUARD_CALLBACK_H */
