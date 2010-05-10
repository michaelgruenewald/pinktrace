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
 * Pink's socket related data structures and functions
 **/

#if defined(PINKTRACE_FREEBSD) || defined(DOXYGEN)
#include <sys/socket.h>
#endif

#include <netinet/in.h>
#include <sys/un.h>

/**
 * This struct defines a decoded socket address.
 **/
typedef struct {
	/** Family of the socket address **/
	int family;

	/**
	 * This union contains type-safe pointers to the real socket address.
	 * Check the family before attempting to obtain the real object.
	 **/
	union {
		/** Padding, for internal use only **/
		char _pad[128];

		/** Socket address, for internal use only **/
		struct sockaddr _sa;

		/** Unix socket address, only valid if family is AF_UNIX. **/
		struct sockaddr_un sa_un;

		/** Inet socket address, only valid if family is AF_INET. **/
		struct sockaddr_in sa_in;

#if PINKTRACE_HAVE_IPV6 || defined(DOXYGEN)
		/**
		 * Inet6 socket address, only valid if family is AF_INET6.
		 * This member is only available if IPV6 support was enabled at
		 * compile time. Check with PINKTRACE_HAVE_IPV6.
		 **/
		struct sockaddr_in6 sa6;
#endif /* PINKTRACE_HAVE_IPV6... */
	} u;
} pink_socket_address_t;

#if defined(PINKTRACE_LINUX) || defined(DOXYGEN)
/**
 * Decoded socket subcalls
 *
 * \note Availability: Linux
 **/
typedef enum {
	/** socket() subcall **/
	PINK_SOCKET_SUBCALL_SOCKET = 1,
	/** bind() subcall **/
	PINK_SOCKET_SUBCALL_BIND,
	/** connect() subcall **/
	PINK_SOCKET_SUBCALL_CONNECT,
	/** listen() subcall **/
	PINK_SOCKET_SUBCALL_LISTEN,
	/** accept() subcall **/
	PINK_SOCKET_SUBCALL_ACCEPT,
	/** getsockname() subcall **/
	PINK_SOCKET_SUBCALL_GETSOCKNAME,
	/** getpeername() subcall **/
	PINK_SOCKET_SUBCALL_GETPEERNAME,
	/** socketpair() subcall **/
	PINK_SOCKET_SUBCALL_SOCKETPAIR,
	/** send() subcall **/
	PINK_SOCKET_SUBCALL_SEND,
	/** recv() subcall **/
	PINK_SOCKET_SUBCALL_RECV,
	/** sendto() subcall **/
	PINK_SOCKET_SUBCALL_SENDTO,
	/** recvfrom() subcall **/
	PINK_SOCKET_SUBCALL_RECVFROM,
	/** shutdown() subcall **/
	PINK_SOCKET_SUBCALL_SHUTDOWN,
	/** setsockopt() subcall **/
	PINK_SOCKET_SUBCALL_SETSOCKOPT,
	/** getsockopt() subcall **/
	PINK_SOCKET_SUBCALL_GETSOCKOPT,
	/** sendmsg() subcall **/
	PINK_SOCKET_SUBCALL_SENDMSG,
	/** recvmsg() subcall **/
	PINK_SOCKET_SUBCALL_RECVMSG,
	/** accept4() subcall **/
	PINK_SOCKET_SUBCALL_ACCEPT4,
} pink_socket_subcall_t;

/**
 * Name the given socket subcall
 *
 * \note Availability: Linux
 *
 * \param subcall The socket subcall
 *
 * \return The name of the subcall
 **/
const char *
pink_name_socket_subcall(pink_socket_subcall_t subcall);

#endif /* defined(PINKTRACE_LINUX)... */

#endif /* !PINKTRACE_GUARD_SOCKET_H */
