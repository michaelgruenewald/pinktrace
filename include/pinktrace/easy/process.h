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

/**
 * \file
 * Pink's easy process representation
 **/

/** Opaque structure which represents a process entry. **/
typedef struct pink_easy_process pink_easy_process_t;

/** Opaque structure which represents a process tree. **/
typedef struct pink_easy_process_tree pink_easy_process_tree_t;

/**
 * Returns the process ID of the entry
 *
 * \param proc Process entry
 *
 * \return Process ID
 **/
pid_t
pink_easy_process_get_pid(const pink_easy_process_t *proc);

/**
 * Returns the bitness of the entry
 *
 * \param proc Process entry
 *
 * \return Bitness
 **/
pink_bitness_t
pink_easy_process_get_bitness(const pink_easy_process_t *proc);

void *
pink_easy_process_get_data(const pink_easy_process_t *proc);

void
pink_easy_process_set_data(pink_easy_process_t *proc);

/**
 * Remove a process from the process tree.
 *
 * \param tree Process tree
 * \param pid Process ID
 *
 * \return true if the element was found and removed, false otherwise.
 **/
PINK_NONNULL(1)
bool
pink_easy_process_tree_remove(pink_easy_process_tree_t *tree, pid_t pid);

/**
 * Returns the count of nodes in the tree
 *
 * \param tree Process tree
 *
 * \return Count of nodes
 **/
unsigned
pink_easy_process_tree_get_count(const pink_easy_process_tree_t *tree);

/**
 * Search the process tree for the given pid.
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
 * \param cb Callback
 * \param userdata User data to pass to the callback
 *
 * \return Number of visited nodes
 **/
PINK_NONNULL(1,2)
unsigned
pink_easy_process_tree_walk(const pink_easy_process_tree_t *tree, bool (*cb) (pink_easy_process_t *proc, void *userdata), void *userdata);

#endif /* !PINKTRACE_EASY_GUARD_PROCESS_H */
