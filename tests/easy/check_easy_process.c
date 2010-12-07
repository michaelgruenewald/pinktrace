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
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "check_pinktrace_easy.h"

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#include <check.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

START_TEST(t_process_tree_new)
{
	pink_easy_process_tree_t *tree;

	tree = pink_easy_process_tree_new();
	fail_if(tree == NULL, "%d(%s)", errno, strerror(errno));
	fail_unless(tree->count == 0, "%d", tree->count);

	free(tree);
}
END_TEST

START_TEST(t_process_tree_insert)
{
	bool ret;
	pink_easy_process_t *proc;
	pink_easy_process_tree_t *tree;

	tree = pink_easy_process_tree_new();
	fail_if(tree == NULL, "%d(%s)", errno, strerror(errno));

	proc = calloc(1, sizeof(pink_easy_process_t));
	fail_if(proc == NULL, "%d(%s)", errno, strerror(errno));

	ret = pink_easy_process_tree_insert(tree, proc);
	fail_unless(ret, "wtf?");

	fail_unless(tree->count == 1, "%d", tree->count);

	ret = pink_easy_process_tree_insert(tree, proc);
	fail_if(ret, "wtf?");

	fail_unless(tree->count == 1, "%d", tree->count);

	free(proc);
	free(tree);
}
END_TEST

START_TEST(t_process_tree_search)
{
	bool ret;
	pink_easy_process_t *proc, *nproc;
	pink_easy_process_tree_t *tree;

	tree = pink_easy_process_tree_new();
	fail_if(tree == NULL, "%d(%s)", errno, strerror(errno));

	proc = calloc(1, sizeof(pink_easy_process_t));
	fail_if(proc == NULL, "%d(%s)", errno, strerror(errno));

	proc->flags = 00001;
	proc->pid = 3;
	proc->bitness = PINK_BITNESS_32;

	ret = pink_easy_process_tree_insert(tree, proc);
	fail_unless(ret, "wtf?");

	nproc = pink_easy_process_tree_search(tree, 3);
	fail_if(nproc == NULL, "wtf?");
	fail_unless(nproc->flags == 00001, "%i", nproc->flags);
	fail_unless(nproc->pid == 3, "%i", nproc->pid);
	fail_unless(nproc->bitness == PINK_BITNESS_32, "%i", nproc->bitness);

	for (unsigned int i = 4; i < 256; i++) {
		nproc = pink_easy_process_tree_search(tree, i);
		fail_unless(nproc == NULL, "%i", i);
	}

	free(proc);
	free(tree);
}
END_TEST

START_TEST(t_process_tree_remove)
{
	bool ret;
	pink_easy_process_t *proc;
	pink_easy_process_tree_t *tree;

	tree = pink_easy_process_tree_new();
	fail_if(tree == NULL, "%d(%s)", errno, strerror(errno));

	proc = calloc(1, sizeof(pink_easy_process_t));
	fail_if(proc == NULL, "%d(%s)", errno, strerror(errno));

	proc->flags = 00001;
	proc->pid = 3;
	proc->bitness = PINK_BITNESS_32;

	ret = pink_easy_process_tree_insert(tree, proc);
	fail_unless(ret, "wtf?");

	ret = pink_easy_process_tree_remove(tree, 5);
	fail_if(ret, "wtf?");

	ret = pink_easy_process_tree_remove(tree, 3);
	fail_unless(ret, "wtf?");

	ret = pink_easy_process_tree_remove(tree, 5);
	fail_if(ret, "wtf?");

	fail_unless(tree->count == 0, "%d", tree->count);

	free(proc);
	free(tree);
}
END_TEST

static bool
_process_tree_walk_func(pink_easy_process_t *proc, void *userdata)
{
	pid_t stop = *((pid_t *)userdata);
	return proc->pid != stop;
}

static bool
_process_tree_free_func(pink_easy_process_t *proc, PINK_UNUSED void *userdata)
{
	if (proc)
		free(proc);
	return true;
}

START_TEST(t_process_tree_walk)
{
	unsigned visited;
	pink_easy_process_t *proc;
	pink_easy_process_tree_t *tree;

	tree = pink_easy_process_tree_new();
	fail_if(tree == NULL, "%d(%s)", errno, strerror(errno));

	for (unsigned int i = 1; i < 1024; i++) {
		proc = malloc(sizeof(pink_easy_process_t));
		fail_if(proc == NULL, "%i: %d(%s)", i, errno, strerror(errno));
		proc->pid = i;
		fail_unless(pink_easy_process_tree_insert(tree, proc), "%i", i);
		fail_unless(tree->count == i, "%i != %i", tree->count, i);
	}

	fail_unless(tree->count == 1023, "%i", tree->count);
	for (unsigned int j = 1; j < 1024; j++) {
		visited = pink_easy_process_tree_walk(tree, _process_tree_walk_func, &j);
		fail_unless(visited == j, "%i != %i", visited, j);
	}

	pink_easy_process_tree_walk(tree, _process_tree_free_func, NULL);
	free(tree);
}
END_TEST

Suite *
easy_process_suite_create(void)
{
	Suite *s = suite_create("easy_process");

	TCase *tc_pink_easy_process = tcase_create("pink_easy_process");

	tcase_add_test(tc_pink_easy_process, t_process_tree_new);
	tcase_add_test(tc_pink_easy_process, t_process_tree_insert);
	tcase_add_test(tc_pink_easy_process, t_process_tree_search);
	tcase_add_test(tc_pink_easy_process, t_process_tree_remove);
	tcase_add_test(tc_pink_easy_process, t_process_tree_walk);

	suite_add_tcase(s, tc_pink_easy_process);

	return s;
}
