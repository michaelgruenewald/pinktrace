/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon strace which is:
 *   Copyright (c) 1991, 1992 Paul Kranenburg <pk@cs.few.eur.nl>
 *   Copyright (c) 1993 Branko Lankester <branko@hacktic.nl>
 *   Copyright (c) 1993, 1994, 1995, 1996 Rick Sladkey <jrs@world.std.com>
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
 **/

/**
 * \defgroup process Pink's easy process representation
 * \ingroup easy
 * \{
 **/

/**
 * \struct pink_easy_process_t
 * \brief Opaque structure which represents a process entry.
 *
 * This entries are allocated internally by the tracing context.
 **/
typedef struct pink_easy_process pink_easy_process_t;

/**
 * \struct pink_easy_process_tree_t
 * \brief Opaque structure which represents a process tree.
 *
 * This tree is allocated internally by the tracing context.
 **/
typedef struct pink_easy_process_tree pink_easy_process_tree_t;

/**
 * Returns the process ID of the entry.
 *
 * \attention There's no setter for pid_t member because pinktrace handles it
 * internally.
 *
 * \param proc Process entry
 *
 * \return Process ID
 **/
PINK_NONNULL(1)
pid_t
pink_easy_process_get_pid(const pink_easy_process_t *proc);

/**
 * Returns the bitness of the entry
 *
 * \attention There's no setter for #pink_bitness_t member because pinktrace
 * handles it internally.
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
 * \param proc Process entry
 *
 * \return User data
 **/
PINK_NONNULL(1)
void *
pink_easy_process_get_data(const pink_easy_process_t *proc);

/**
 * Returns the count of entries in the tree.
 *
 * \param tree Process tree
 *
 * \return Number of entries in the tree
 **/
unsigned
pink_easy_process_tree_get_count(const pink_easy_process_tree_t *tree);

/**
 * Remove a process from the process tree.
 *
 * \note pinktrace doesn't export an insertion function because insertions are
 * handled internally by this library. You may, however, need to remove an
 * entry due to problems (e.g. -ESRCH) caused by the process.
 *
 * \param tree Process tree
 * \param pid Process ID
 *
 * \return true if process entry was found and removed, false otherwise
 **/
PINK_NONNULL(1)
bool
pink_easy_process_tree_remove(pink_easy_process_tree_t *tree, pid_t pid);

/**
 * Search the process tree for the given process ID.
 *
 * \param tree The process tree
 * \param pid Process ID
 *
 * \return The process on successful lookup, NULL on failure.
 **/
PINK_NONNULL(1)
pink_easy_process_t *
pink_easy_process_tree_search(const pink_easy_process_tree_t *tree, pid_t pid);

/**
 * Walk the process tree.
 *
 * \param tree Process tree
 * \param func Walk function
 * \param userdata User data to pass to the walk function
 *
 * \return Number of visited entries
 **/
PINK_NONNULL(1,2)
unsigned
pink_easy_process_tree_walk(const pink_easy_process_tree_t *tree, pink_easy_walk_func_t func, void *userdata);

/**
 * \}
 **/

#endif /* !PINKTRACE_EASY_GUARD_PROCESS_H */
