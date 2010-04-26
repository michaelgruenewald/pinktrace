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

#ifndef PINKTRACE_GUARD_SOCKET_H
#define PINKTRACE_GUARD_SOCKET_H 1

/**
 * \file
 * Pink's socket call decoders
 **/

#include <stdbool.h>
#include <sys/types.h>

#include <netinet/in.h>
#include <sys/un.h>

#include <pinktrace/bitness.h>

/**
 * \struct pink_sockaddr_t
 *
 * This opaque structure represents a decoded socket address.
 **/
typedef struct pink_sockaddr pink_sockaddr_t;

/**
 * Free the given socket address
 *
 * \param addr The socket address
 **/
void
pink_sockaddr_free(pink_sockaddr_t *addr);

/**
 * Return the family of the address.
 *
 * \param addr The socket address
 *
 * \return The address family. One of:
 *  - -1 (Which means the address family is unsupported)
 *  - AF_UNIX
 *  - AF_INET
 *  - AF_INET6 (If IPV6 support was enabled at compile time)
 **/
int
pink_sockaddr_get_family(const pink_sockaddr_t *addr);

/**
 * Return the UNIX socket address
 *
 * \note This address is only valid if pink_sockaddr_get_family() returned
 * AF_UNIX.
 *
 * \param addr The socket address
 *
 * \return The UNIX socket address
 **/
const struct sockaddr_un *
pink_sockaddr_get_unix(const pink_sockaddr_t *addr);

/**
 * Return the inet socket address
 *
 * \note This address is only valid if pink_sockaddr_get_family() returned
 * AF_INET.
 *
 * \param addr The socket address
 *
 * \return The inet socket address
 **/
const struct sockaddr_in *
pink_sockaddr_get_inet(const pink_sockaddr_t *addr);

/**
 * Return the inet6 socket address
 *
 * \note This address is only valid if pink_sockaddr_get_family() returned
 * AF_INET6.
 *
 * \param addr The socket address
 *
 * \return The inet6 socket address, this may be casted to a
 * (struct sockaddr_in6 *). If IPV6 support wasn't enabled at compile time, this
 * function always returns NULL.
 **/
const void *
pink_sockaddr_get_inet6(const pink_sockaddr_t *addr);

#endif /* !PINKTRACE_GUARD_SOCKET_H */
