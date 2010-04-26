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

#ifndef PINKTRACE_GUARD_DECODE_H
#define PINKTRACE_GUARD_DECODE_H 1

/**
 * \file
 * Pink's system call decoders
 **/

#include <stdbool.h>
#include <sys/types.h>

#include <pinktrace/bitness.h>
#include <pinktrace/socket.h>

/**
 * Decoded socket subcalls
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
 * Get the data in argument arg and place it in dest.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param arg The number of the argument (0-5)
 * \param dest Pointer to store the data
 * \param len Length of the data
 **/
bool
pink_decode_simple(pid_t pid, pink_bitness_t bitness, int arg, void *dest, size_t len);

/**
 * Get the string argument and place it in dest.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param arg The number of the argument (0-5)
 * \param dest Pointer to store the string
 * \param len Length of the string
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_string(pid_t pid, pink_bitness_t bitness, int arg, char *dest, size_t len);

/**
 * Like pink_decode_string() but allocates the string itself.
 *
 * \return The path on success, NULL on failure and sets errno
 * accordingly.
 **/
char *
pink_decode_string_persistent(pid_t pid, pink_bitness_t bitness, int arg);

/**
 * Decode the socket call and place it in call.
 *
 * \note This function decodes the socketcall(2) system call on some
 * architectures. On others it's equivalent to pink_util_get_syscall().
 *
 * \see pink_socket_subcall_t
 *
 * \param pid Process ID of the child whose argument is to be received
 * \param bitness Bitness of the child
 * \param call The pointer to store the decoded socket call
 * \param decoded If non-NULL, store whether the socketcall(2) system call has
 * been decoded
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_socket_call(pid_t pid, pink_bitness_t bitness, long *call, bool *decoded);

/**
 * Get the socket file descriptor in argument arg and place it in fd.
 *
 * \note This function decodes the socketcall(2) system call on some
 * architectures.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param arg The number of the argument (Only 0 makes sense)
 * \param fd The pointer to store the socket file descriptor
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_socket_fd(pid_t pid, pink_bitness_t bitness, int arg, long *fd);

/**
 * Return the socket address in argument arg, decode as needed.
 *
 * \note This function decodes the socketcall(2) system call on some
 * architectures.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param arg The number of the argument. One of:
 *  - 1 (for connect, bind etc.)
 *  - 4 (for sendto)
 * \param fd The pointer to store the socket file descriptor that resides in
 * argument one with index zero.
 *
 * \return The socket address on success, NULL on failure and sets errno
 * accordingly. The caller must free the return value using
 * pink_sockaddr_free().
 **/
pink_sockaddr_t *
pink_decode_socket_address(pid_t pid, pink_bitness_t bitness, int arg, long *fd);

#endif /* !PINKTRACE_GUARD_DECODE_H */
