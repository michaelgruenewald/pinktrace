/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 *
 * This file is part of Pink's Tracing Library. pinktrace is free software; you
 * can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License version 2.1, as published by the Free Software
 * Foundation.
 *
 * pinktrace is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <string.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

/**
 * Internal tracing/decoding functions common among architectures.
 **/

bool
pink_internal_decode_socket_address(pid_t pid, long addr, long addrlen,
	pink_socket_address_t *addr_r)
{
	if (addr == 0) {
		/* NULL */
		addr_r->family = -1;
		return true;
	}
	if (addrlen < 2 || (unsigned long)addrlen > sizeof(addr_r->u))
		addrlen = sizeof(addr_r->u);

	memset(&addr_r->u, 0, sizeof(addr_r->u));
	if (!pink_util_moven(pid, addr, addr_r->u._pad, addrlen))
		return false;
	addr_r->u._pad[sizeof(addr_r->u._pad) - 1] = '\0';

	addr_r->family = addr_r->u._sa.sa_family;
	return true;
}
