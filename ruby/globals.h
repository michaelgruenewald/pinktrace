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

#ifndef PINKTRACE_GUARD_RUBY_GLOBALS_H
#define PINKTRACE_GUARD_RUBY_GLOBALS_H 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <pinktrace/pink.h>

/* undef bunch of defines that ruby-1.8 defines in its standard headers */
#ifdef PACKAGE_NAME
#undef PACKAGE_NAME
#endif
#ifdef PACKAGE_TARNAME
#undef PACKAGE_TARNAME
#endif
#ifdef PACKAGE_VERSION
#undef PACKAGE_VERSION
#endif
#ifdef PACKAGE_STRING
#undef PACKAGE_STRING
#endif
#ifdef PACKAGE_BUGREPORT
#undef PACKAGE_BUGREPORT
#endif
#include <ruby.h>

#ifndef INET_ADDRSTRLEN
#define INET_ADDRSTRLEN 16
#endif

#if PINKTRACE_HAVE_IPV6
#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif
#endif

/*
 * `pid_t' requires special attention as its size varies a lot between
 * different architectures.
 */
#if !defined(SIZEOF_PID_T) || SIZEOF_PID_T == SIZEOF_INT
#ifndef PIDT2NUM
#define PIDT2NUM(p) INT2FIX((p))
#endif
#ifndef NUM2PIDT
#define NUM2PIDT(p) NUM2INT((p))
#endif
#elif SIZEOF_PID_T == SIZEOF_LONG
#ifndef PIDT2NUM
#define PIDT2NUM(p) LONG2NUM((p))
#endif
#ifndef NUM2PIDT
#define NUM2PIDT(p) NUM2LONG((p))
#endif
#elif defined(SIZEOF_LONG_LONG) && SIZEOF_PID_T == SIZEOF_LONG_LONG
#ifndef PIDT2NUM
#define PIDT2NUM(p) LL2NUM((p))
#endif
#ifndef NUM2PIDT
#define NUM2PIDT(p) NUM2LL((p))
#endif
#else
#error "sizeof(pid_t) is neither sizeof(int), sizeof(long) or sizeof(long long)"
#endif

#ifdef PINKTRACE_LINUX
#define IS_ABSTRACT(addr) ((addr)->u.sa_un.sun_path[0] == '\0' && (addr)->u.sa_un.sun_path[1] != '\0')
#else
#define IS_ABSTRACT(addr) 0
#endif

#ifndef RSTRING_LEN
#define RSTRING_LEN(v) (RSTRING((v))->len)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif

VALUE pinkrb_cAddress;

VALUE pinkrb_trace_me(VALUE mod);
VALUE pinkrb_trace_cont(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_resume(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_kill(VALUE mod, VALUE vpid);
VALUE pinkrb_trace_singlestep(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_syscall(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_syscall_entry(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_syscall_exit(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_sysemu(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_sysemu_singlestep(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_geteventmsg(VALUE mod, VALUE vpid);
VALUE pinkrb_trace_setup(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_trace_attach(VALUE mod, VALUE vpid);
VALUE pinkrb_trace_detach(int argc, VALUE *argv, VALUE mod);

VALUE pinkrb_event_decide(int argc, VALUE *argv, VALUE mod);

VALUE pinkrb_bitness_get(VALUE mod, VALUE vpid);
VALUE pinkrb_bitness_name(VALUE mod, VALUE vbit);
VALUE pinkrb_bitness_wordsize(VALUE mod, VALUE vbit);

VALUE pinkrb_name_syscall(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_name_lookup(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_util_get_syscall(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_util_set_syscall(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_util_get_return(VALUE mod, VALUE vpid);
VALUE pinkrb_util_set_return(VALUE mod, VALUE vpid, VALUE vret);
VALUE pinkrb_util_get_arg(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_util_set_arg(int argc, VALUE *argv, VALUE mod);

VALUE pinkrb_decode_string(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_encode_string_safe(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_encode_string(int argc, VALUE *argv, VALUE mod);

VALUE pinkrb_decode_strarray(int argc, VALUE *argv, VALUE mod);

VALUE pinkrb_has_socketcall(VALUE mod, int argc, VALUE *argv);
VALUE pinkrb_name_socket_subcall(VALUE mod, VALUE vsubcall);
VALUE pinkrb_decode_socket_call(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_decode_socket_fd(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_decode_socket_address(int argc, VALUE *argv, VALUE mod);
VALUE pinkrb_decode_socket_address_fd(int argc, VALUE *argv, VALUE mod);

VALUE pinkrb_Address_family(VALUE self);
VALUE pinkrb_Address_length(VALUE self);
VALUE pinkrb_Address_is_unix(VALUE self);
VALUE pinkrb_Address_is_inet(VALUE self);
VALUE pinkrb_Address_is_inet6(VALUE self);
VALUE pinkrb_Address_is_netlink(VALUE self);
VALUE pinkrb_Address_is_abstract(VALUE self);
VALUE pinkrb_Address_path(VALUE self);
VALUE pinkrb_Address_port(VALUE self);
VALUE pinkrb_Address_ip(VALUE self);
VALUE pinkrb_Address_ipv6(VALUE self);
VALUE pinkrb_Address_pid(VALUE self);
VALUE pinkrb_Address_groups(VALUE self);

void Init_PinkTrace(void);

#endif /* !PINKTRACE_GUARD_RUBY_GLOBALS_H */
