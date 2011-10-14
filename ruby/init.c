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
 * Document-module: PinkTrace
 *
 * Ruby extension to the <tt>pinktrace</tt> library.
 *
 * <tt>pinktrace</tt> is a lightweight library which provides a robust API for
 * tracing processes.
 *
 * == Classes
 *
 * Following are the classes that are most likely to be of interest to the user:
 *
 * - PinkTrace::Trace
 * - PinkTrace::Event
 * - PinkTrace::Bitness
 * - PinkTrace::Syscall
 * - PinkTrace::String
 * - PinkTrace::StringArray
 * - PinkTrace::Socket
 *
 * == Exceptions
 *
 * Errno::* errors are raised in case of ptrace errors.
 * Check ptrace(2) manual page for more information.
 */

void
Init_PinkTrace(void)
{
	VALUE mod;
	VALUE bitness_mod;
	VALUE event_mod;
	VALUE string_mod;
	VALUE strarray_mod;
	VALUE socket_mod;
	VALUE syscall_mod;
	VALUE trace_mod;

	mod = rb_define_module("PinkTrace");
	/*
	 * Document-const: PinkTrace::HAVE_IPV6
	 *
	 * This constant is +true+ if IPV6 support is available
	 */
	rb_define_const(mod, "HAVE_IPV6", PINKTRACE_HAVE_IPV6 ? Qtrue : Qfalse);
	/*
	 * Document-const: PinkTrace::HAVE_NETLINK
	 *
	 * This constant is +true+ if Netlink support is available
	 */
	rb_define_const(mod, "HAVE_NETLINK", PINKTRACE_HAVE_NETLINK ? Qtrue : Qfalse);
	/*
	 * Document-const: PinkTrace::PACKAGE
	 *
	 * The name of the package (eg pinktrace)
	 */
	rb_define_const(mod, "PACKAGE", rb_str_new2(PINKTRACE_PACKAGE));
	/*
	 * Document-const: PinkTrace::VERSION
	 *
	 * The version, two digits per part (eg 1.3.5 -> 10305)
	 */
	rb_define_const(mod, "VERSION", INT2FIX(PINKTRACE_VERSION));
	/*
	 * Document-const: PinkTrace::VERSION_MAJOR
	 *
	 * The major version (eg 0.4.1 -> 0)
	 */
	rb_define_const(mod, "VERSION_MAJOR", INT2FIX(PINKTRACE_VERSION_MAJOR));
	/*
	 * Document-const: PinkTrace::VERSION_MINOR
	 *
	 * The minor version (eg 0.4.1 -> 4)
	 */
	rb_define_const(mod, "VERSION_MINOR", INT2FIX(PINKTRACE_VERSION_MINOR));
	/*
	 * Document-const: PinkTrace::VERSION_MICRO
	 *
	 * The micro version (eg 0.4.1 -> 1)
	 */
	rb_define_const(mod, "VERSION_MICRO", INT2FIX(PINKTRACE_VERSION_MICRO));
	/*
	 * Document-const: PinkTrace::VERSION_SUFFIX
	 *
	 * The version suffix (eg "_alpha1"), often an empty string
	 */
	rb_define_const(mod, "VERSION_SUFFIX", rb_str_new2(PINKTRACE_VERSION_SUFFIX));
	/*
	 * Document-const: PinkTrace::GIT_HEAD
	 *
	 * The Git head used to build this binary, if applicable (eg "deadbeef" or "1.0.0-40-f00-dirty" or "")
	 */
	rb_define_const(mod, "GIT_HEAD", rb_str_new2(PINKTRACE_GIT_HEAD));
	/*
	 * Document-const: PinkTrace::PC_SLOT
	 *
	 * The suffix used for so names (eg "0.30" or "0.31_15ece615")
	 */
	rb_define_const(mod, "PC_SLOT", rb_str_new2(PINKTRACE_PC_SLOT));

	/*
	 * Document-module: PinkTrace::Trace
	 *
	 * This class includes thin wrappers around the <tt>ptrace()</tt> system call.
	 */
	trace_mod = rb_define_module_under(mod, "Trace");
#ifdef PINKTRACE_LINUX
	/*
	 * Document-const: PinkTrace::Trace::OPTION_SYSGOOD
	 * (Availability: Linux)
	 *
	 * This constant represents the trace option +SYSGOOD+. If this flag is
	 * set in the options argument of PinkTrace::Trace.setup, when
	 * delivering syscall traps, bit 7 is set in signal number (i.e.,
	 * deliver (SIGTRAP | 0x80) This makes it easy for the tracer to tell
	 * the difference between normal traps and those caused by a sycall.
	 * This option may not work on all architectures.
	 */
	rb_define_const(trace_mod, "OPTION_SYSGOOD", INT2FIX(PINK_TRACE_OPTION_SYSGOOD));
	/*
	 * Document-const: PinkTrace::Trace::OPTION_FORK
	 * (Availability: Linux)
	 *
	 * This constant represents the trace option +FORK+. If this flag is set
	 * in the options argument of PinkTrace::Trace.setup, stop the child at
	 * the next fork(2) call with (SIGTRAP | PTRACE_EVENT_FORK << 8) and
	 * automatically start tracing the newly forked process, which will
	 * start with a SIGSTOP. The PID for the new process can be retrieved
	 * with PinkTrace::Trace.geteventmsg.
	 */
	rb_define_const(trace_mod, "OPTION_FORK", INT2FIX(PINK_TRACE_OPTION_FORK));
	/*
	 * Document-const: PinkTrace::Trace::OPTION_VFORK
	 * (Availability: Linux)
	 *
	 * This constant represents the trace option +VFORK+. If this flag is
	 * set in the options argument of PinkTrace::Trace.setup, stop the
	 * child at the next vfork(2) call with
	 * (SIGTRAP | PTRACE_EVENT_VFORK << 8) and automatically start tracing
	 * the newly vforked process, which will start with a SIGSTOP. The PID
	 * for the new process can be retrieved with
	 * PinkTrace::Trace.geteventmsg.
	 */
	rb_define_const(trace_mod, "OPTION_VFORK", INT2FIX(PINK_TRACE_OPTION_VFORK));
	/*
	 * Document-const: PinkTrace::Trace::OPTION_CLONE
	 * (Availability: Linux)
	 *
	 * This constant represnets the trace option +CLONE+. If this flag is
	 * set in the options argument of PinkTrace::Trace.setup, stop the
	 * child at the next clone(2) call with
	 * (SIGTRAP | PTRACE_EVENT_CLONE << 8) and automatically start tracing
	 * the newly cloned process, which will start with a SIGSTOP. The PID
	 * for the new process can be retrieved with
	 * PinkTrace::Trace.geteventmsg.
	 */
	rb_define_const(trace_mod, "OPTION_CLONE", INT2FIX(PINK_TRACE_OPTION_CLONE));
	/*
	 * Document-const: PinkTrace::Trace::OPTION_EXEC
	 * (Availability: Linux)
	 *
	 * This constant represents the trace option +EXEC+. If this flag is set
	 * in the options argument of PinkTrace::Trace.setup, stop the child at
	 * the next <tt>execve</tt>(2) call with
	 * (SIGTRAP | PTRACE_EVENT_EXEC << 8).
	 */
	rb_define_const(trace_mod, "OPTION_EXEC", INT2FIX(PINK_TRACE_OPTION_EXEC));
	/*
	 * Document-const: PinkTrace::Trace::OPTION_VFORK_DONE
	 * (Availability: Linux)
	 *
	 * This constant represents the trace option +VFORK_DONE+. If this flag
	 * is set in the options argument of PinkTrace::Trace.setup, stop the
	 * child at the completion of the next vfork(2) call with
	 * (SIGTRAP | PTRACE_EVENT_VFORK_DONE << 8).
	 */
	rb_define_const(trace_mod, "OPTION_VFORK_DONE", INT2FIX(PINK_TRACE_OPTION_VFORK_DONE));
	/*
	 * Document-const: PinkTrace::Trace::OPTION_EXIT
	 * (Availability: Linux)
	 *
	 * This constant represents the trace option +EXIT+. If this flag is set
	 * in the options argument of PinkTrace::Trace.setup, stop the child at
	 * exit with (SIGTRAP | PTRACE_EVENT_EXIT << 8).  This child's exit
	 * status can be retrieved with PinkTrace::Trace.geteventmsg.  This
	 * stop will be done early during process exit when registers are still
	 * available, allowing the tracer to see where the exit occured,
	 * whereas the normal exit notification is done after the process is
	 * finished exiting. Even though context is available, the tracer
	 * cannot prevent the exit from happening at this point.
	 */
	rb_define_const(trace_mod, "OPTION_EXIT", INT2FIX(PINK_TRACE_OPTION_EXIT));
	/*
	 * Document-const: PinkTrace::Trace::OPTION_EXIT
	 * (Availability: Linux)
	 *
	 * This constant represents all option flags bitwise OR'ed together.
	 */
	rb_define_const(trace_mod, "OPTION_ALL", INT2FIX(PINK_TRACE_OPTION_ALL));
#endif
	rb_define_module_function(trace_mod, "me", pinkrb_trace_me, 0); /* in trace.c */
	rb_define_module_function(trace_mod, "cont", pinkrb_trace_cont, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "resume", pinkrb_trace_resume, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "kill", pinkrb_trace_kill, 1); /* in trace.c */
	rb_define_module_function(trace_mod, "singlestep", pinkrb_trace_singlestep, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "syscall", pinkrb_trace_syscall, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "syscall_entry", pinkrb_trace_syscall_entry, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "syscall_exit", pinkrb_trace_syscall_exit, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "sysemu", pinkrb_trace_sysemu, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "sysemu_singlestep", pinkrb_trace_sysemu_singlestep, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "geteventmsg", pinkrb_trace_geteventmsg, 1); /* in trace.c */
	rb_define_module_function(trace_mod, "setup", pinkrb_trace_setup, -1); /* in trace.c */
	rb_define_module_function(trace_mod, "attach", pinkrb_trace_attach, 1); /* in trace.c */
	rb_define_module_function(trace_mod, "detach", pinkrb_trace_detach, -1); /* in trace.c */

	/*
	 * Document-module: PinkTrace::Event
	 *
	 * This class defines constants and functions for event decisions.
	 */
	event_mod = rb_define_module_under(mod, "Event");
	/*
	 * Document-const: PinkTrace::Event::EVENT_STOP
	 *
	 * The traced child has received a SIGSTOP.
	 */
	rb_define_const(event_mod, "EVENT_STOP", INT2FIX(PINK_EVENT_STOP));
	/*
	 * Document-const: PinkTrace::Event::EVENT_TRAP
	 *
	 * The traced child has received a SIGTRAP.
	 */
	rb_define_const(event_mod, "EVENT_TRAP", INT2FIX(PINK_EVENT_TRAP));
	/*
	 * Document-const: PinkTrace::Event::EVENT_SYSCALL
	 *
	 * The traced child is entering or exiting a system call.
	 */
	rb_define_const(event_mod, "EVENT_SYSCALL", INT2FIX(PINK_EVENT_SYSCALL));
	/*
	 * Document-const: PinkTrace::Event::EVENT_FORK
	 *
	 * The traced child called fork(2).
	 */
	rb_define_const(event_mod, "EVENT_FORK", INT2FIX(PINK_EVENT_FORK));
	/*
	 * Document-const: PinkTrace::Event::EVENT_VFORK
	 *
	 *   The traced child called vfork(2).
	 */
	rb_define_const(event_mod, "EVENT_VFORK", INT2FIX(PINK_EVENT_VFORK));
	/*
	 * Document-const: PinkTrace::Event::EVENT_CLONE
	 *
	 * The traced child called clone(2).
	 */
	rb_define_const(event_mod, "EVENT_CLONE", INT2FIX(PINK_EVENT_CLONE));
	/*
	 * Document-const: PinkTrace::Event::EVENT_VFORK_DONE
	 *
	 * The traced child is exiting a vfork(2) call.
	 */
	rb_define_const(event_mod, "EVENT_VFORK_DONE", INT2FIX(PINK_EVENT_VFORK_DONE));
	/*
	 * Document-const: PinkTrace::Event::EVENT_EXEC
	 *
	 * The traced child has called execve(2).
	 */
	rb_define_const(event_mod, "EVENT_EXEC", INT2FIX(PINK_EVENT_EXEC));
	/*
	 * Document-const: PinkTrace::Event::EVENT_EXIT
	 *
	 * The traced child is exiting. (ptrace way, stopped before exit)
	 */
	rb_define_const(event_mod, "EVENT_EXIT", INT2FIX(PINK_EVENT_EXIT));
	/*
	 * Document-const: PinkTrace::Event::EVENT_GENUINE
	 *
	 * The traced child has received a genuine signal.
	 */
	rb_define_const(event_mod, "EVENT_GENUINE", INT2FIX(PINK_EVENT_GENUINE));
	/*
	 * Document-const: PinkTrace::Event::EVENT_EXIT_GENUINE
	 *
	 * The traced child has exited normally.
	 */
	rb_define_const(event_mod, "EVENT_EXIT_GENUINE", INT2FIX(PINK_EVENT_EXIT_GENUINE));
	/*
	 * Document-const: PinkTrace::Event::EVENT_EXIT_SIGNAL
	 *
	 * The traced child has been terminated with a signal.
	 */
	rb_define_const(event_mod, "EVENT_EXIT_SIGNAL", INT2FIX(PINK_EVENT_EXIT_SIGNAL));
	/*
	 * Document-const: PinkTrace::Event::EVENT_UNKNOWN
	 *
	 * Unknown event
	 */
	rb_define_const(event_mod, "EVENT_UNKNOWN", INT2FIX(PINK_EVENT_UNKNOWN));
	rb_define_module_function(event_mod, "decide", pinkrb_event_decide, -1); /* in event.c */

	/*
	 * Document-module: PinkTrace::Bitness
	 *
	 * This class defines constants and functions about bitness.
	 */
	bitness_mod = rb_define_module_under(mod, "Bitness");
	/*
	 * Document-const: PinkTrace::Bitness::COUNT_SUPPORTED
	 *
	 * Count of supported bitnesses (eg 2 on x86_64, 1 on i386)
	 */
	rb_define_const(bitness_mod, "COUNT_SUPPORTED", INT2FIX(PINKTRACE_BITNESS_COUNT_SUPPORTED));
	/*
	 * Document-const: PinkTrace::Bitness::DEFAULT
	 *
	 * The default bitness
	 */
	rb_define_const(bitness_mod, "DEFAULT", INT2FIX(PINKTRACE_BITNESS_DEFAULT));
	/*
	 * Document-const: PinkTrace::Bitness::BITNESS_32
	 *
	 * 32 bit mode
	 */
	rb_define_const(bitness_mod, "BITNESS_32", INT2FIX(PINK_BITNESS_32));
	/*
	 * Document-const: PinkTrace::Bitness::BITNESS_64
	 *
	 * 64 bit mode
	 */
	rb_define_const(bitness_mod, "BITNESS_64", INT2FIX(PINK_BITNESS_64));
	/*
	 * Document-const: PinkTrace::Bitness::BITNESS_32_SUPPORTED
	 *
	 * +true+ if 32 bit is supported, +false+ otherwise.
	 */
	rb_define_const(bitness_mod, "BITNESS_32_SUPPORTED", PINKTRACE_BITNESS_32_SUPPORTED ? Qtrue : Qfalse);
	/*
	 * Document-const: PinkTrace::Bitness::BITNESS_64_SUPPORTED
	 *
	 * +true+ if 64 bit is supported, +false+ otherwise.
	 */
	rb_define_const(bitness_mod, "BITNESS_64_SUPPORTED", PINKTRACE_BITNESS_64_SUPPORTED ? Qtrue : Qfalse);
	rb_define_module_function(bitness_mod, "get", pinkrb_bitness_get, 1); /* in bitness.c */
	rb_define_module_function(bitness_mod, "name", pinkrb_bitness_name, 1); /* in bitness.c */
	rb_define_module_function(bitness_mod, "wordsize", pinkrb_bitness_wordsize, 1); /* in bitness.c */

	/*
	 * Document-module: PinkTrace::Syscall
	 *
	 * This class defines utilities useful when tracing processes.
	 */
	syscall_mod = rb_define_module_under(mod, "Syscall");
	/*
	 * Document-const: PinkTrace::Syscall::INVALID
	 *
	 * This constant is an invalid system call number. You may use this
	 * constant as an argument to PinkTrace::Syscall.set_no to deny a
	 * system call from execution.
	 */
	rb_define_const(syscall_mod, "INVALID", INT2FIX(PINKTRACE_INVALID_SYSCALL));
	/*
	 * Document-const: PinkTrace::Syscall::MAX_INDEX
	 *
	 * The index arguments of system call functions must be smaller than
	 * this constant.
	 */
	rb_define_const(syscall_mod, "MAX_INDEX", INT2FIX(PINK_MAX_INDEX));
	rb_define_module_function(syscall_mod, "name", pinkrb_name_syscall, -1); /* in syscall.c */
	rb_define_module_function(syscall_mod, "lookup", pinkrb_name_lookup, -1); /* in syscall.c */
	rb_define_module_function(syscall_mod, "get_no", pinkrb_util_get_syscall, -1); /* in syscall.c */
	rb_define_module_function(syscall_mod, "set_no", pinkrb_util_set_syscall, -1); /* in syscall.c */
	rb_define_module_function(syscall_mod, "get_ret", pinkrb_util_get_return, 1); /* in syscall.c */
	rb_define_module_function(syscall_mod, "set_ret", pinkrb_util_set_return, 2); /* in syscall.c */
	rb_define_module_function(syscall_mod, "get_arg", pinkrb_util_get_arg, -1); /* in syscall.c */
	rb_define_module_function(syscall_mod, "set_arg", pinkrb_util_set_arg, -1); /* in syscall.c */
	/*
	 * Document-module: PinkTrace::String
	 *
	 * This class contains functions to decode/encode string arguments.
	 */
	string_mod = rb_define_module_under(mod, "String");
	rb_define_module_function(string_mod, "decode", pinkrb_decode_string, -1); /* in string.c */
	rb_define_module_function(string_mod, "encode", pinkrb_encode_string_safe, -1); /* in string.c */
	rb_define_module_function(string_mod, "encode!", pinkrb_encode_string, -1); /* in string.c */

	 /*
	 * Document-module: PinkTrace::StringArray
	 *
	 * This class contains functions to decode NULL-terminated string array members.
	 */
	strarray_mod = rb_define_module_under(mod, "StringArray");
	rb_define_module_function(strarray_mod, "decode", pinkrb_decode_strarray, -1); /* in strarray.c */

	/*
	 * Document-module: PinkTrace::Socket
	 *
	 * This module includes functions for decoding socket calls.
	 */
	socket_mod = rb_define_module_under(mod, "Socket");
	rb_define_module_function(socket_mod, "has_socketcall?", pinkrb_has_socketcall, -1); /* in socket.c */
	rb_define_module_function(socket_mod, "name", pinkrb_name_socket_subcall, 1); /* in socket.c */
	rb_define_module_function(socket_mod, "decode_call", pinkrb_decode_socket_call, -1); /* in socket.c */
	rb_define_module_function(socket_mod, "decode_fd", pinkrb_decode_socket_fd, -1); /* in socket.c */

	/*
	 * Document-class: PinkTrace::Socket::Address
	 *
	 * This class represents a decoded socket address.
	 */
	pinkrb_cAddress = rb_define_class_under(socket_mod, "Address", rb_cObject);
	rb_undef_method(pinkrb_cAddress, "initialize");
	rb_define_method(pinkrb_cAddress, "family", pinkrb_Address_family, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "length", pinkrb_Address_length, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "unix?", pinkrb_Address_is_unix, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "inet?", pinkrb_Address_is_inet, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "inet6?", pinkrb_Address_is_inet6, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "netlink?", pinkrb_Address_is_netlink, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "abstract?", pinkrb_Address_is_abstract, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "path", pinkrb_Address_path, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "port", pinkrb_Address_port, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "ip", pinkrb_Address_ip, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "ipv6", pinkrb_Address_ipv6, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "pid", pinkrb_Address_pid, 0); /* in socket.c */
	rb_define_method(pinkrb_cAddress, "groups", pinkrb_Address_groups, 0); /* in socket.c */

	rb_define_module_function(socket_mod, "decode_address", pinkrb_decode_socket_address, -1); /* in socket.c */
	rb_define_module_function(socket_mod, "decode_address_fd", pinkrb_decode_socket_address_fd, -1); /* in socket.c */
}
