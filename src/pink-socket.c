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

#include <stdbool.h>
#include <stdlib.h>

#include <netinet/in.h>
#include <sys/un.h>

#include <pinktrace/internal.h>
#include <pinktrace/pink.h>

void
pink_sockaddr_free(pink_sockaddr_t *addr)
{
	free(addr);
}

int
pink_sockaddr_get_family(const pink_sockaddr_t *addr)
{
	return addr->family;
}

const struct sockaddr_un *
pink_sockaddr_get_unix(const pink_sockaddr_t *addr)
{
	return &addr->u.sa_un;
}

const struct sockaddr_in *
pink_sockaddr_get_inet(const pink_sockaddr_t *addr)
{
	return &addr->u.sa_in;
}

#if PINKTRACE_HAVE_IPV6
const struct sockaddr_in6 *
pink_sockaddr_get_inet6(const pink_sockaddr_t *addr)
{
	return &addr->u.sa6;
}
#endif /* PINKTRACE_HAVE_IPV6 */
