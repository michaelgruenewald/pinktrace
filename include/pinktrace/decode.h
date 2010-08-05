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
 * Get the data in argument arg and place it in dest.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param ind The index of the argument (0-5, see #PINK_MAX_INDEX)
 * \param dest Pointer to store the data
 * \param len Length of the data
 **/
bool
pink_decode_simple(pid_t pid, pink_bitness_t bitness, unsigned ind, void *dest, size_t len);

/**
 * Get the string argument and place it in dest.
 *
 * \note On FreeBSD this function is equivalent to pink_decode_simple().
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param ind The index of the argument (0-5, see #PINK_MAX_INDEX)
 * \param dest Pointer to store the string
 * \param len Length of the string
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_string(pid_t pid, pink_bitness_t bitness, unsigned ind, char *dest, size_t len);

/**
 * Like pink_decode_string() but allocates the string itself.
 *
 * \return The path on success, NULL on failure and sets errno
 * accordingly.
 **/
char *
pink_decode_string_persistent(pid_t pid, pink_bitness_t bitness, unsigned ind);

#if defined(PINKTRACE_LINUX) || defined(DOXYGEN)
/**
 * Decode the socket call and place it in call.
 *
 * \note Availability: Linux
 * \note This function decodes the socketcall(2) system call on some
 * architectures. On others it's equivalent to pink_util_get_syscall().
 *
 * \see pink_socket_subcall_t
 *
 * \param pid Process ID of the child whose argument is to be received
 * \param bitness Bitness of the child
 * \param subcall_r The pointer to store the decoded socket call
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_socket_call(pid_t pid, pink_bitness_t bitness, long *subcall_r);

/**
 * Get the socket file descriptor in argument arg and place it in fd.
 *
 * \note Availability: Linux
 * \note This function decodes the socketcall(2) system call on some
 * architectures.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param ind The index of the argument (Only 0 makes sense)
 * \param fd The pointer to store the socket file descriptor
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_socket_fd(pid_t pid, pink_bitness_t bitness, unsigned ind, long *fd);
#endif /* defined(PINKTRACE_LINUX)... */

/**
 * Get the socket address and place it in addr_r.
 *
 * \note This function decodes the socketcall(2) system call on some
 * architectures.
 *
 * \note If the address argument of the system call was NULL, this function
 * returns true and sets addr->family to -1.
 *
 * \param pid Process ID of the child whose argument is to be received.
 * \param bitness Bitness of the child
 * \param ind The index of the argument. One of:
 *  - 1 (for connect, bind etc.)
 *  - 4 (for sendto)
 * \param fd_r The pointer to store the socket file descriptor that resides in
 * argument one with index zero. Set this to NULL if you don't need the file
 * descriptor to be decoded.
 * \param addr_r The pointer to store the decoded socket address
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_decode_socket_address(pid_t pid, pink_bitness_t bitness, unsigned ind,
	long *fd_r, pink_socket_address_t *addr_r);

#endif /* !PINKTRACE_GUARD_DECODE_H */
