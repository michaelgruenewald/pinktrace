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

#include <pinktrace/pink.h>

const char *
pink_name_socket_subcall(pink_socket_subcall_t subcall)
{
	switch (subcall) {
	case PINK_SOCKET_SUBCALL_SOCKET:
		return "socket";
	case PINK_SOCKET_SUBCALL_BIND:
		return "bind";
	case PINK_SOCKET_SUBCALL_CONNECT:
		return "connect";
	case PINK_SOCKET_SUBCALL_LISTEN:
		return "listen";
	case PINK_SOCKET_SUBCALL_ACCEPT:
		return "accept";
	case PINK_SOCKET_SUBCALL_GETSOCKNAME:
		return "getsockname";
	case PINK_SOCKET_SUBCALL_GETPEERNAME:
		return "getpeername";
	case PINK_SOCKET_SUBCALL_SOCKETPAIR:
		return "socketpair";
	case PINK_SOCKET_SUBCALL_SEND:
		return "send";
	case PINK_SOCKET_SUBCALL_RECV:
		return "recv";
	case PINK_SOCKET_SUBCALL_SENDTO:
		return "sendto";
	case PINK_SOCKET_SUBCALL_RECVFROM:
		return "recvfrom";
	case PINK_SOCKET_SUBCALL_SHUTDOWN:
		return "shutdown";
	case PINK_SOCKET_SUBCALL_SETSOCKOPT:
		return "setsockopt";
	case PINK_SOCKET_SUBCALL_GETSOCKOPT:
		return "getsockopt";
	case PINK_SOCKET_SUBCALL_SENDMSG:
		return "sendmsg";
	case PINK_SOCKET_SUBCALL_RECVMSG:
		return "recvmsg";
	case PINK_SOCKET_SUBCALL_ACCEPT4:
		return "accept4";
	default:
		return "unknown";
	}
}
