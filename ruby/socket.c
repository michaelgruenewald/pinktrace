/*
 * Copyright (c) 2010, 2011 Ali Polatel <polatel@gmail.com>
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

#include "globals.h"

/*
 * Document-method: PinkTrace::Socket.has_socketcall?
 * call-seq:
 *   PinkTrace::Socket.has_socketcall?([bitness=PinkTrace::Bitness::DEFAULT]) -> true or false
 *
 * Returns true if the socket calls - like connect, bind, sendto etc. - are
 * implemented as subcalls of the socketcall(2) system call, false otherwise.
 *
 * Availability: Linux
 */
VALUE
pinkrb_has_socketcall(VALUE mod, int argc, VALUE *argv)
{
#ifdef PINKTRACE_LINUX
	unsigned bit;
	VALUE vbit;

	switch (rb_scan_args(argc, argv, "01", &vbit)) {
	case 0:
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 1:
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}

	return pink_has_socketcall(bit) ? Qtrue : Qfalse;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Socket.name
 * call-seq:
 *   PinkTrace::Socket.name(subcall) -> String or nil
 *
 * Returns a string representation of the socket subcall.
 *
 * Availability: Linux
 */
VALUE
pinkrb_name_socket_subcall(VALUE mod, VALUE vsubcall)
{
#ifdef PINKTRACE_LINUX
	unsigned subcall;
	const char *subname;

	subcall = FIX2UINT(vsubcall);
	subname = pink_name_socket_subcall(subcall);
	return subname ? rb_str_new2(subname) : Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Socket.decode_call
 * call-seq:
 *   PinkTrace::Socket.decode_call(pid, [bitness=PinkTrace::Bitness::DEFAULT]) -> fixnum
 *
 * Returns the decoded socket call.
 *
 * Note: This function decodes the socketcall(2) system call on some
 * architectures. On others it's equivalent to PinkTrace::Syscall.get_no
 *
 * Availability: Linux
 */
VALUE
pinkrb_decode_socket_call(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_LINUX
	pid_t pid;
	unsigned bit;
	long subcall;
	VALUE vpid, vbit;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vbit)) {
	case 1:
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 2:
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_decode_socket_call(pid, bit, &subcall))
		rb_sys_fail("pink_decode_socket_call()");

	return LONG2NUM(subcall);
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Socket.decode_fd
 * call-seq:
 *   PinkTrace::Socket.decode_fd(pid, [[index=0], [bitness=PinkTrace::Bitness::DEFAULT]]) -> fixnum
 *
 * Returns the socket file descriptor.
 *
 * Note: This function decodes the socketcall(2) system call on some
 * architectures.
 *
 * Availability: Linux
 */
VALUE
pinkrb_decode_socket_fd(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_LINUX
	pid_t pid;
	unsigned bit, ind;
	long fd;
	VALUE vpid, vind, vbit;

	switch (rb_scan_args(argc, argv, "12", &vpid, &vind, &vbit)) {
	case 1:
		bit = PINKTRACE_BITNESS_DEFAULT;
		ind = 0;
		break;
	case 2:
		bit = PINKTRACE_BITNESS_DEFAULT;
		ind = FIX2UINT(vind);
		break;
	case 3:
		bit = FIX2UINT(vbit);
		ind = FIX2UINT(vind);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_decode_socket_fd(pid, bit, ind, &fd))
		rb_sys_fail("pink_decode_socket_fd()");

	return LONG2NUM(fd);
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Socket.decode_address
 * call-seq:
 *   PinkTrace::Socket.decode_address(pid, index, [bitness=PinkTrace::Bitness::DEFAULT]) -> addr
 *
 * Decodes the socket address at the given index.
 * If the system call's address argument was NULL, this function sets
 * +addr.family+ to -1.
 */
VALUE
pinkrb_decode_socket_address(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	pink_socket_address_t *addr;
	VALUE vpid, vind, vbit;
	VALUE addrObj;

	switch (rb_scan_args(argc, argv, "21", &vpid, &vind, &vbit)) {
	case 2:
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 3:
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);
	ind = FIX2UINT(vind);

	addrObj = Data_Make_Struct(pinkrb_cAddress, pink_socket_address_t, NULL, free, addr);

	if (!pink_decode_socket_address(pid, bit, ind, NULL, addr))
		rb_sys_fail("pink_decode_socket_address()");

	return addrObj;
}

/*
 * Document-method: PinkTrace::Socket.decode_address_fd
 * call-seq:
 *   PinkTrace::Socket.decode_address_fd(pid, index, [bitness=PinkTrace::Bitness::DEFAULT]) -> addr, fd
 *
 * Decodes the socket address at the given index and the file descriptor at index 0.
 * If the system call's address argument was NULL this function sets
 * +addr.family+ to -1.
 */
VALUE
pinkrb_decode_socket_address_fd(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	long fd;
	pink_socket_address_t *addr;
	VALUE vpid, vind, vbit;
	VALUE addrObj;

	switch (rb_scan_args(argc, argv, "21", &vpid, &vind, &vbit)) {
	case 2:
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 3:
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);
	ind = FIX2UINT(vind);

	addrObj = Data_Make_Struct(pinkrb_cAddress, pink_socket_address_t, NULL, free, addr);

	if (!pink_decode_socket_address(pid, bit, ind, &fd, addr))
		rb_sys_fail("pink_decode_socket_address()");

	return rb_assoc_new(addrObj, LONG2NUM(fd));
}

/*
 * Document-method: family
 * call-seq:
 *   addr.family -> fixnum
 *
 * Returns the family of the address.
 */
VALUE
pinkrb_Address_family(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);

	return INT2FIX(addr->family);
}

/*
 * Document-method: length
 * call-seq:
 *   addr.length -> fixnum
 *
 * Returns the length of the address.
 */
VALUE
pinkrb_Address_length(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);

	return INT2FIX(addr->length);
}

/*
 * Document-method: unix?
 * call-seq:
 *   addr.unix? -> true or false
 *
 * Returns true if the address is of family +AF_UNIX+.
 */
VALUE
pinkrb_Address_is_unix(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	return (addr->family == AF_UNIX) ? Qtrue : Qfalse;
}

/*
 * Document-method: inet?
 * call-seq:
 *   addr.inet? -> true or false
 *
 * Returns true if the address is of family +AF_INET+.
 */
VALUE
pinkrb_Address_is_inet(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	return (addr->family == AF_INET) ? Qtrue : Qfalse;
}

/*
 * Document-method: inet6?
 * call-seq:
 *   addr.inet6? -> true or false
 *
 * Returns true if the address is of family +AF_INET6+.
 */
VALUE
pinkrb_Address_is_inet6(VALUE self)
{
#if PINKTRACE_HAVE_IPV6
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	return (addr->family == AF_INET6) ? Qtrue : Qfalse;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: netlink?
 * call-seq:
 *   addr.netlink? -> true or false
 *
 * Returns true if the address is of family +AF_NETLINK+.
 */
VALUE
pinkrb_Address_is_netlink(VALUE self)
{
#if PINKTRACE_HAVE_NETLINK
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	return (addr->family == AF_NETLINK) ? Qtrue : Qfalse;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: abstract?
 * call-seq:
 *   addr.abstract? -> true or false
 *
 * Returns true if the address is an abstract UNIX socket address.
 */
VALUE
pinkrb_Address_is_abstract(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family == AF_UNIX && IS_ABSTRACT(addr))
		return Qtrue;
	return Qfalse;
}

/*
 * Document-method: path
 * call-seq:
 *   addr.path -> String
 *
 * Returns the path of the UNIX socket address.
 */
VALUE
pinkrb_Address_path(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family != AF_UNIX)
		rb_raise(rb_eTypeError, "Invalid family");
	return rb_str_new2(addr->u.sa_un.sun_path + (IS_ABSTRACT(addr) ? 1 : 0));
}

/*
 * Document-method: port
 * call-seq:
 *   addr.port -> Fixnum
 *
 * Returns the port of an Inet or Inet6 socket address
 */
VALUE
pinkrb_Address_port(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	switch (addr->family) {
	case AF_INET:
		return INT2FIX(ntohs(addr->u.sa_in.sin_port));
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
		return INT2FIX(ntohs(addr->u.sa6.sin6_port));
#endif
	default:
		rb_raise(rb_eTypeError, "Invalid family");
	}
}

/*
 * Document-method: ip
 * call-seq:
 *   addr.ip -> String
 *
 * Returns the address of an Inet socket address as a string
 */
VALUE
pinkrb_Address_ip(VALUE self)
{
	char *ip;
	pink_socket_address_t *addr;
	VALUE ret;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family != AF_INET)
		rb_raise(rb_eTypeError, "Invalid family");
	ip = ALLOC_N(char, INET_ADDRSTRLEN);
	inet_ntop(AF_INET, &addr->u.sa_in.sin_addr, ip, INET_ADDRSTRLEN);
	ret = rb_str_new2(ip);
	free(ip);
	return ret;
}

/*
 * Document-method: ipv6
 * call-seq:
 *   addr.ipv6 -> String
 *
 * Returns the IPV6 address of an Inet6 socket address as string.
 */
VALUE
pinkrb_Address_ipv6(VALUE self)
{
#if PINKTRACE_HAVE_IPV6
	char *ip;
	pink_socket_address_t *addr;
	VALUE ret;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family != AF_INET6)
		rb_raise(rb_eTypeError, "Invalid family");
	ip = ALLOC_N(char, INET6_ADDRSTRLEN);
	inet_ntop(AF_INET6, &addr->u.sa6.sin6_addr, ip, INET6_ADDRSTRLEN);
	ret = rb_str_new2(ip);
	free(ip);
	return ret;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: pid
 * call-seq:
 *   addr.pid -> Fixnum
 *
 * Returns the process ID of the netlink socket address
 */
VALUE
pinkrb_Address_pid(VALUE self)
{
#if PINKTRACE_HAVE_NETLINK
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family == AF_NETLINK)
		return PIDT2NUM(addr->u.nl.nl_pid);
#endif
	rb_raise(rb_eNotImpError, "Not implemented");
}

/*
 * Document-method: groups
 * call-seq:
 *   addr.groups -> Fixnum
 *
 * Returns the mcast groups mask of the netlink socket address
 */
VALUE
pinkrb_Address_groups(VALUE self)
{
#if PINKTRACE_HAVE_NETLINK
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family == AF_NETLINK)
		return LONG2NUM(addr->u.nl.nl_groups);
#endif
	rb_raise(rb_eNotImpError, "Not implemented");
}
