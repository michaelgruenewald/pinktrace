/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010, 2011 Ali Polatel <alip@exherbo.org>
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

#ifndef PINKTRACE_EASY_GUARD_PROCESS_H
#define PINKTRACE_EASY_GUARD_PROCESS_H 1

#include <stdbool.h>
#include <sys/types.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/func.h>

/**
 * \file
 * \brief Pink's easy process representation
 *
 * \ingroup g_easy_process
 **/

/**
 * \struct pink_easy_process_t
 * \brief Opaque structure which represents a process entry.
 *
 * \ingroup g_easy_process
 *
 * These entries are allocated internally by the tracing context.
 **/
typedef struct pink_easy_process pink_easy_process_t;

/**
 * \struct pink_easy_process_tree_t
 * \brief Opaque structure which represents a process list.
 *
 * \ingroup g_easy_process
 *
 * This list is maintained internally by the tracing context.
 **/
typedef struct pink_easy_process_list pink_easy_process_list_t;

/**
 * Returns the process ID of the entry.
 *
 * \ingroup g_easy_process
 *
 * \param proc Process entry
 *
 * \return Process ID
 **/
PINK_NONNULL(1)
pid_t
pink_easy_process_get_pid(const pink_easy_process_t *proc);

/**
 * Returns the process ID of this entry's parent or -1 for the eldest entries.
 *
 * \ingroup g_easy_process
 *
 * \param proc Process entry
 *
 * \return Parent Process ID or -1
 **/
PINK_NONNULL(1)
pid_t
pink_easy_process_get_ppid(const pink_easy_process_t *proc);

/**
 * Returns the bitness of the entry
 *
 * \ingroup g_easy_process
 *
 * \param proc Process entry
 *
 * \return Bitness
 **/
PINK_NONNULL(1)
pink_bitness_t
pink_easy_process_get_bitness(const pink_easy_process_t *proc);

/**
 * Set the user data of the process entry.
 *
 * \ingroup g_easy_process
 *
 * \note This function accepts a destructor function pointer which may be used
 * to free the user data. You may pass NULL if you want to handle the
 * destruction yourself or use the standard free() function from stdlib.h for
 * basic destruction.
 *
 * \param proc Process entry
 * \param data User data
 * \param func The destructor function of the user data
 **/
PINK_NONNULL(1)
void
pink_easy_process_set_data(pink_easy_process_t *proc, void *data, pink_easy_free_func_t func);

/**
 * Get the user data of the process entry, previously set by
 * pink_easy_process_set_data()
 *
 * \ingroup g_easy_process
 *
 * \param proc Process entry
 *
 * \return User data
 **/
PINK_NONNULL(1)
void *
pink_easy_process_get_data(const pink_easy_process_t *proc);

/**
 * Remove a process from the process list.
 *
 * \ingroup g_easy_process
 *
 * \note pinktrace doesn't export an insertion function because insertions are
 * handled internally by this library. You may, however, need to remove an
 * entry due to problems (e.g. -ESRCH) caused by the process.
 *
 * \param list Process list
 * \param proc Process entry
 **/
PINK_NONNULL(1)
void
pink_easy_process_list_remove(pink_easy_process_list_t *list, const pink_easy_process_t *proc);

/**
 * Look up the process list for the given process ID.
 *
 * \ingroup g_easy_process
 *
 * \param list The process list
 * \param pid Process ID
 *
 * \return The process on successful look up, NULL on failure.
 **/
PINK_NONNULL(1)
pink_easy_process_t *
pink_easy_process_list_lookup(const pink_easy_process_list_t *list, pid_t pid);

/**
 * Walk the process tree.
 *
 * \ingroup g_easy_process
 *
 * \param list Process list
 * \param func Walk function
 * \param userdata User data to pass to the walk function
 *
 * \return Number of visited entries
 **/
PINK_NONNULL(1,2)
unsigned
pink_easy_process_list_walk(const pink_easy_process_list_t *list, pink_easy_walk_func_t func, void *userdata);

#endif /* !PINKTRACE_EASY_GUARD_PROCESS_H */
