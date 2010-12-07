/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 * Based in part upon libdict which is:
 *   Copyright (C) 2001-2010 Farooq Mela.
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <pinktrace/pink.h>
#include <pinktrace/easy/internal.h>
#include <pinktrace/easy/pink.h>

#define CMP_PID(a, b) ((a) < (b)) ? -1 : (((a) > (b)) ? 1 : 0)

#define RB_BLK 0
#define RB_RED 1

static pink_easy_process_t _null = { 0, 0, PINK_BITNESS_UNKNOWN, NULL, RB_BLK, NULL, NULL, NULL };
#define RB_NULL &_null

pid_t
pink_easy_process_get_pid(const pink_easy_process_t *proc)
{
	return proc->pid;
}

pink_bitness_t
pink_easy_process_get_bitness(const pink_easy_process_t *proc)
{
	return proc->bitness;
}

void *
pink_easy_process_get_data(const pink_easy_process_t *proc)
{
	return proc->data;
}

void
pink_easy_process_set_data(pink_easy_process_t *proc, void *data)
{
	proc->data = data;
}

unsigned
pink_easy_process_tree_get_count(const pink_easy_process_tree_t *tree)
{
	return tree->count;
}

static pink_easy_process_t *
pink_easy_process_tree_node_min(pink_easy_process_t *node)
{
	while (node->lnode != RB_NULL)
		node = node->lnode;
	return node;
}

#if 0
static pink_easy_process_t *
pink_easy_process_tree_node_max(pink_easy_process_t *node)
{
	while (node->rnode != RB_NULL)
		node = node->rnode;
	return node;
}
#endif

static pink_easy_process_t *
pink_easy_process_tree_node_next(pink_easy_process_t *node)
{
	pink_easy_process_t *temp;

	assert(node != NULL);

	if (node->rnode != RB_NULL) {
		for (node = node->rnode; node->lnode != RB_NULL; node = node->lnode)
			/* void */ ;
	}
	else {
		temp = node->parent;
		while (temp != RB_NULL && temp->rnode == node) {
			node = temp;
			temp = temp->parent;
		}
		node = temp;
	}

	return node;
}

static void
pink_easy_process_tree_rot_left(pink_easy_process_tree_t *tree, pink_easy_process_t *node)
{
	pink_easy_process_t *rnode, *parent;

	assert(tree != NULL);
	assert(node != NULL);

	rnode = node->rnode;
	node->rnode = rnode->lnode;
	if (rnode->lnode != RB_NULL)
		rnode->lnode->parent = node;
	parent = node->parent;
	rnode->parent = parent;
	if (parent != RB_NULL) {
		if (parent->lnode == node)
			parent->lnode = rnode;
		else
			parent->rnode = rnode;
	}
	else
		tree->root = rnode;

	rnode->lnode = node;
	node->parent = rnode;
}

static void
pink_easy_process_tree_rot_right(pink_easy_process_tree_t *tree, pink_easy_process_t *node)
{
	pink_easy_process_t *lnode, *parent;

	assert(tree != NULL);
	assert(node != NULL);

	lnode = node->lnode;
	node->lnode = lnode->rnode;
	if (lnode->rnode != RB_NULL)
		lnode->rnode->parent = node;
	parent = node->parent;
	lnode->parent = parent;
	if (parent != RB_NULL) {
		if (parent->lnode == node)
			parent->lnode = lnode;
		else
			parent->rnode = lnode;
	}
	else
		tree->root = lnode;

	lnode->rnode = node;
	node->parent = lnode;
}

static void
pink_easy_process_tree_insert_fixup(pink_easy_process_tree_t *tree, pink_easy_process_t *node)
{
	pink_easy_process_t *temp;

	assert(tree != NULL);
	assert(node != NULL);

	while (node != tree->root && node->parent->colour == RB_RED) {
		if (node->parent == node->parent->parent->lnode) {
			temp = node->parent->parent->rnode;
			if (temp->colour == RB_RED) {
				temp->colour = RB_BLK;
				node = node->parent;
				node->colour = RB_BLK;
				node = node->parent;
				node->colour = RB_RED;
			}
			else {
				if (node == node->parent->rnode) {
					node = node->parent;
					pink_easy_process_tree_rot_left(tree, node);
				}
				temp = node->parent;
				temp->colour = RB_BLK;
				temp = temp->parent;
				temp->colour = RB_RED;
				pink_easy_process_tree_rot_right(tree, temp);
			}
		}
		else {
			temp = node->parent->parent->lnode;
			if (temp->colour == RB_RED) {
				temp->colour = RB_BLK;
				node = node->parent;
				node->colour = RB_BLK;
				node = node->parent;
				node->colour = RB_RED;
			}
			else {
				if (node == node->parent->lnode) {
					node = node->parent;
					pink_easy_process_tree_rot_right(tree, node);
				}
				temp = node->parent;
				temp->colour = RB_BLK;
				temp = temp->parent;
				temp->colour = RB_RED;
				pink_easy_process_tree_rot_left(tree, temp);
			}
		}
	}

	tree->root->colour = RB_BLK;
}

static void
pink_easy_process_tree_delete_fixup(pink_easy_process_tree_t *tree, pink_easy_process_t *node)
{
	pink_easy_process_t *temp;

	assert(tree != NULL);
	assert(node != NULL);

	while (node != tree->root && node->colour == RB_BLK) {
		if (node->parent->lnode == node) {
			temp = node->parent->rnode;
			if (temp->colour == RB_RED) {
				temp->colour = RB_BLK;
				node->parent->colour = RB_RED;
				pink_easy_process_tree_rot_left(tree, node->parent);
				temp = node->parent->rnode;
			}
			if (temp->lnode->colour == RB_BLK && temp->rnode->colour == RB_BLK) {
				temp->colour = RB_RED;
				node = node->parent;
			}
			else {
				if (temp->rnode->colour == RB_BLK) {
					temp->lnode->colour = RB_BLK;
					temp->colour = RB_RED;
					pink_easy_process_tree_rot_right(tree, temp);
					temp = node->parent->rnode;
				}
				temp->colour = node->parent->colour;
				temp->rnode->colour = RB_BLK;
				node->parent->colour = RB_BLK;
				pink_easy_process_tree_rot_left(tree, node->parent);
				break;
			}
		} else {
			temp = node->parent->lnode;
			if (temp->colour == RB_RED) {
				temp->colour = RB_BLK;
				node->parent->colour = RB_RED;
				pink_easy_process_tree_rot_right(tree, node->parent);
				temp = node->parent->lnode;
			}
			if (temp->rnode->colour == RB_BLK && temp->lnode->colour == RB_BLK) {
				temp->colour = RB_RED;
				node = node->parent;
			}
			else {
				if (temp->lnode->colour == RB_BLK) {
					temp->rnode->colour = RB_BLK;
					temp->colour = RB_RED;
					pink_easy_process_tree_rot_left(tree, temp);
					temp = node->parent->lnode;
				}
				temp->colour = node->parent->colour;
				node->parent->colour = RB_BLK;
				temp->lnode->colour = RB_BLK;
				pink_easy_process_tree_rot_right(tree, node->parent);
				break;
			}
		}
	}

	node->colour = RB_BLK;
}

pink_easy_process_tree_t *
pink_easy_process_tree_new(void)
{
	pink_easy_process_tree_t *tree;

	tree = malloc(sizeof(pink_easy_process_tree_t));
	if (!tree)
		return NULL;

	tree->root = RB_NULL;
	tree->count = 0;

	return tree;
}

bool
pink_easy_process_tree_insert(pink_easy_process_tree_t *tree, pink_easy_process_t *proc)
{
	int cmp;
	pink_easy_process_t *node, *parent;

	assert(tree != NULL);
	assert(proc != NULL);

	parent = RB_NULL;
	node = tree->root;

	cmp = 0;
	while (node != RB_NULL) {
		cmp = CMP_PID(proc->pid, node->pid);
		if (cmp == -1) {
			parent = node;
			node = node->lnode;
		}
		else if (cmp == 1) {
			parent = node;
			node = node->rnode;
		}
		else
			return false;
	}

	/* Initialize internal data */
	proc->colour = RB_RED;
	proc->parent = parent;
	proc->lnode = RB_NULL;
	proc->rnode = RB_NULL;

	if (proc->parent == RB_NULL) {
		tree->root = proc;
		assert(tree->count == 0);
		tree->count = 1;
		proc->colour = RB_BLK;
		return true;
	}

	if (cmp < 0)
		parent->lnode = proc;
	else
		parent->rnode = proc;

	pink_easy_process_tree_insert_fixup(tree, proc);
	++tree->count;
	return true;
}

pink_easy_process_t *
pink_easy_process_tree_search(const pink_easy_process_tree_t *tree, pid_t pid)
{
	int cmp;
	pink_easy_process_t *node;

	assert(tree != NULL);

	node = tree->root;
	while (node != RB_NULL) {
		cmp = CMP_PID(pid, node->pid);
		if (cmp == -1)
			node = node->lnode;
		else if (cmp == 1)
			node = node->rnode;
		else
			return node;
	}

	return NULL;
}

bool
pink_easy_process_tree_remove(pink_easy_process_tree_t *tree, pid_t pid)
{
	int cmp;
	pink_easy_process_t *node, *temp, *out, *parent;

	assert(tree != NULL);

	node = tree->root;
	while (node != RB_NULL) {
		cmp = CMP_PID(pid, node->pid);
		if (cmp == -1)
			node = node->lnode;
		else if (cmp == 1)
			node = node->rnode;
		else
			break;
	}

	if (node == RB_NULL)
		return false;

	if (node->lnode == RB_NULL || node->rnode == RB_NULL)
		out = node;
	else {
		for (out = node->rnode; out->lnode != RB_NULL; out = out->lnode)
			/* void */ ;

		short tmp_flags;
		tmp_flags = node->flags;
		node->flags = out->flags;
		out->flags = tmp_flags;

		pid_t tmp_pid;
		tmp_pid = node->pid;
		node->pid = out->pid;
		out->pid = tmp_pid;

		pink_bitness_t tmp_bit;
		tmp_bit = node->bitness;
		node->bitness = out->bitness;
		out->bitness = tmp_bit;
	}

	temp = out->lnode != RB_NULL ? out->lnode : out->rnode;
	parent = out->parent;
	temp->parent = parent;
	if (parent != RB_NULL) {
		if (parent->lnode == out)
			parent->lnode = temp;
		else
			parent->rnode = temp;
	}
	else
		tree->root = temp;

	if (out->colour == RB_BLK)
		pink_easy_process_tree_delete_fixup(tree, temp);

	--tree->count;
	return true;
}

unsigned
pink_easy_process_tree_walk(const pink_easy_process_tree_t *tree, bool (*cb) (pink_easy_process_t *proc, void *userdata), void *userdata)
{
	unsigned count;
	pink_easy_process_t *node;

	assert(tree != NULL);
	assert(cb != NULL);

	if (tree->root == RB_NULL)
		return 0;

	count = 0;
	for (node = pink_easy_process_tree_node_min(tree->root); node != RB_NULL; node = pink_easy_process_tree_node_next(node)) {
		++count;
		if (!cb(node, userdata))
			break;
	}
	return count;
}
