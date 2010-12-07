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

#ifndef PINKTRACE_EASY_GUARD_ERROR_H
#define PINKTRACE_EASY_GUARD_ERROR_H 1

/**
 * \file
 * Pink's easy error codes
 **/

/** Child error codes **/
typedef enum {
	PINK_EASY_CERROR_SETUP,
	PINK_EASY_CERROR_EXEC,
} pink_easy_cerror_t;

/** Error codes **/
typedef enum {
	PINK_EASY_ERROR_SUCCESS = 0,
	PINK_EASY_ERROR_CALLBACK_ABORT,
	PINK_EASY_ERROR_MALLOC_ELDEST,
	PINK_EASY_ERROR_FORK,
	PINK_EASY_ERROR_VFORK,
	PINK_EASY_ERROR_BITNESS_ELDEST,
	PINK_EASY_ERROR_SETUP_ELDEST,
	PINK_EASY_ERROR_STEP_INITIAL,
	PINK_EASY_ERROR_WAIT_ALL,
	PINK_EASY_ERROR_WAIT,
	PINK_EASY_ERROR_MALLOC_PREMATURE_CHILD,
	PINK_EASY_ERROR_BITNESS_PREMATURE_CHILD,
	PINK_EASY_ERROR_STEP_AFTER_STOP,
	PINK_EASY_ERROR_STEP_AFTER_SYSCALL,
	PINK_EASY_ERROR_STEP_AFTER_EXIT,
	PINK_EASY_ERROR_GETEVENTMSG_FORK,
	PINK_EASY_ERROR_MALLOC_NEW_CHILD,
	PINK_EASY_ERROR_SETUP_NEW_CHILD,
	PINK_EASY_ERROR_STEP_PREMATURE,
	PINK_EASY_ERROR_STEP_AFTER_FORK,
	PINK_EASY_ERROR_STEP_AFTER_EXEC,
	PINK_EASY_ERROR_BITNESS_EXEC,
	PINK_EASY_ERROR_STEP_AFTER_GENUINE,
	PINK_EASY_ERROR_GETEVENTMSG_EXIT,
	PINK_EASY_ERROR_EVENT_UNKNOWN,
	PINK_EASY_ERROR_MAX,
} pink_easy_error_t;

#endif /* !PINKTRACE_EASY_GUARD_ERROR_H */
