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

#include <stdlib.h>
#include <string.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

/**
 * Internal tracing/decoding functions common among architectures.
 **/

pink_sockaddr_t *
pink_internal_decode_socket_address(pid_t pid, long addr, long addrlen)
{
	pink_sockaddr_t *res;

	res = malloc(sizeof(pink_sockaddr_t));
	if (!res)
		return NULL;

	if (addr == 0) {
		/* Unknown family */
		res->family = -1;
		return res;
	}
	if (addrlen < 2 || (unsigned long)addrlen > sizeof(res->u))
		addrlen = sizeof(res->u);

	memset(&res->u, 0, sizeof(res->u));
	if (!pink_util_moven(pid, addr, res->u.pad, addrlen)) {
		free(res);
		return NULL;
	}
	res->u.pad[sizeof(res->u.pad) - 1] = '\0';

	switch (res->u.sa.sa_family) {
	case AF_UNIX:
		res->family = AF_UNIX;
		break;
	case AF_INET:
		res->family = AF_INET;
		break;
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
		res->family = AF_INET6;
		break;
#endif /* PINKTRACE_HAVE_IPV6 */
	default:
		res->family = -1;
		break;
	}

	return res;
}
