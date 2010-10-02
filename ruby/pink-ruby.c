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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <errno.h>
#include <stdlib.h> /* free() */
#include <string.h> /* memcpy() */

#include <netinet/in.h> /* INET{,6}_ADDRSTRLEN */
#include <arpa/inet.h> /* inet_ntop() */

#include <pinktrace/pink.h>
#include <ruby.h>

#ifndef INET_ADDRSTRLEN
#define INET_ADDRSTRLEN 16
#endif /* !INET_ADDRSTRLEN */

#if PINKTRACE_HAVE_IPV6
#ifndef INET6_ADDRSTRLEN
#define INET6_ADDRSTRLEN 46
#endif /* !INET6_ADDRSTRLEN */
#endif

#ifndef PIDT2NUM
#define PIDT2NUM(p) LONG2NUM((p))
#endif

#define IS_ABSTRACT(addr) \
	((addr)->u.sa_un.sun_path[0] == '\0' \
	 && (addr)->u.sa_un.sun_path[1] != '\0')

void
Init_PinkTrace(void);

static VALUE pinkrb_eBitnessError;
static VALUE pinkrb_eIndexError;

static VALUE pinkrb_cAddress;

static void
check_bitness(unsigned bit)
{
	switch (bit) {
	case PINK_BITNESS_64:
#if defined(I386) || defined(POWERPC)
		rb_raise(pinkrb_eBitnessError, "Unsupported bitness");
#endif
		break;
	case PINK_BITNESS_32:
#if defined(IA64) || defined(POWERPC64)
		rb_raise(pinkrb_eBitnessError, "Unsupported bitness");
#endif
		break;
	default:
		rb_raise(pinkrb_eBitnessError, "Undefined bitness");
		break;
	}
}

static void
check_index(unsigned ind)
{
	if (ind >= PINK_MAX_INDEX)
		rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
}

/*
 * Document-class: PinkTrace
 *
 * == Summary
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
 * - PinkTrace::Socket
 *
 * == Constants
 *
 * - PinkTrace::PACKAGE
 *
 *   The name of the package (eg pinktrace)
 *
 * - PinkTrace::VERSION_MAJOR
 *
 *   The major version (eg 0.4.1 -> 0)
 *
 * - PinkTrace::VERSION_MINOR
 *
 *   The minor version (eg 0.4.1 -> 4)
 *
 * - PinkTrace::VERSION_MICRO
 *
 *   The micro version (eg 0.4.1 -> 1)
 *
 * - PinkTrace::VERSION
 *
 *   The version, two digits per part (eg 1.3.5 -> 10305)
 *
 * - PinkTrace::VERSION_SUFFIX
 *
 *   The version suffix (eg "_alpha1"), often an empty string
 *
 * - PinkTrace::GIT_HEAD
 *
 *   The Git head used to build this binary, if applicable (eg "deadbeef" or "1.0.0-40-f00-dirty" or "")
 *
 * - PinkTrace::PC_SLOT
 *
 *   The suffix used for so names (eg "0.30" or "0.31_15ece615")
 *
 * - PinkTrace::HAVE_IPV6
 *
 *   This constant can be used to figure out if IPV6 support was compiled in.
 *   +false+ if IPV6 support isn't available, +true+ otherwise.
 *
 * == Exceptions
 *
 * - PinkTrace::BitnessError
 * - PinkTrace::IndexError
 */

/*
 * Document-class: PinkTrace::BitnessError
 *
 * Raised when the given bitness argument is either unsupported or undefined.
 */

/*
 * Document-class: PinkTrace::IndexError
 *
 * Raised when an index argument is not smaller than PinkTrace::Syscall::MAX_INDEX.
 */

/*
 * Document-class: PinkTrace::Trace
 *
 * This class includes thin wrappers around the <tt>ptrace()</tt> system call.
 *
 * == Constants
 *
 * Note: The constants below are only available on Linux.
 *
 * - PinkTrace::Trace::OPTION_SYSGOOD
 *
 *   This constant represents the trace option SYSGOOD.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   when delivering syscall traps, bit 7 is set in signal number (i.e.,
 *   deliver (SIGTRAP | 0x80) This makes it easy for the tracer to tell the
 *   difference between normal traps and those caused by a sycall. This
 *   option may not work on all architectures.
 *
 * - PinkTrace::Trace::OPTION_FORK
 *
 *   This constant represents the trace option FORK.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next fork(2) call with (SIGTRAP | PTRACE_EVENT_FORK << 8)
 *   and automatically start tracing the newly forked process, which will start with
 *   a SIGSTOP. The PID for the new process can be retrieved with PinkTrace::Trace.geteventmsg.
 *
 * - PinkTrace::Trace::OPTION_VFORK
 *
 *   This constant represents the trace option VFORK.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next vfork(2) call with (SIGTRAP | PTRACE_EVENT_VFORK << 8)
 *   and automatically start tracing the newly vforked process, which will start with
 *   a SIGSTOP. The PID for the new process can be retrieved with PinkTrace::Trace.geteventmsg.
 *
 * - PinkTrace::Trace::OPTION_CLONE
 *
 *   This constant represnets the trace option CLONE.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next clone(2) call with (SIGTRAP | PTRACE_EVENT_CLONE << 8)
 *   and automatically start tracing the newly cloned process, which will start with
 *   a SIGSTOP. The PID for the new process can be retrieved with PinkTrace::Trace.geteventmsg.
 *
 * - PinkTrace::Trace::OPTION_EXEC
 *
 *   This constant represents the trace option EXEC.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next execve(2) call with (SIGTRAP | PTRACE_EVENT_EXEC << 8).
 *
 * - PinkTrace::Trace::OPTION_VFORK_DONE
 *
 *   This constant represents the trace option VFORK_DONE.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the completion of the next vfork(2) call with
 *   (SIGTRAP | PTRACE_EVENT_VFORK_DONE << 8).
 *
 * - PinkTrace::Trace::OPTION_EXIT
 *
 *   This constant represents the trace option EXIT.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at exit with (SIGTRAP | PTRACE_EVENT_EXIT << 8).
 *   This child's exit status can be retrieved with PinkTrace::Trace.geteventmsg.
 *   This stop will be done early during process exit when registers are still available,
 *   allowing the tracer to see where the exit occured, whereas the normal exit
 *   notification is done after the process is finished exiting. Even though
 *   context is available, the tracer cannot prevent the exit from happening at
 *   this point.
 *
 * - PinkTrace::Trace::OPTION_ALL
 *
 *   This constant represents all option flags bitwise OR'ed together.
 *
 * == Exceptions
 *
 * Errno::* errors are raised in case of ptrace errors.
 * Check ptrace(2) manual page for more information.
 */

/*
 * Document-method: PinkTrace::Trace.me
 * call-seq: PinkTrace::Trace.me() => nil
 *
 * Indicates that this process is to be traced by its parent. Any signal
 * (except SIGKILL) delivered to this process will cause it to stop and its
 * parent to be notified via Process.wait. Also, all subsequent calls to
 * execve(2) by this process will cause a SIGTRAP to be sent to it, giving the
 * parent a chance to gain control before the new program begins execution.
 *
 * Note: This function is used only by the child process; the rest are used
 * only by the parent.
 */
static VALUE
pinkrb_trace_me(PINK_UNUSED VALUE mod)
{
	if (!pink_trace_me())
		rb_sys_fail("pink_trace_me()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.cont
 * call-seq: PinkTrace::Trace.cont(pid, [[sig=0], [addr=1]]) => nil
 *
 * Restarts the stopped child process.
 *
 * If +sig+ argument is non-zero and not SIGSTOP, it is interpreted as the
 * signal to be delivered to the child; otherwise, no signal is delivered.
 * Thus, for example, the parent can control whether a signal sent to the child
 * is delivered or not.
 *
 * On FreeBSD +addr+ is an address specifying the place where execution
 * is to be resumed (a new value for the program counter), or 1 to indicate
 * that execution is to pick up where it left off.
 * On Linux, this argument is not used.
 */
static VALUE
pinkrb_trace_cont(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	long sig, addr;

	if (argc < 1 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			sig = FIX2LONG(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		sig = 0;

	if (argc == 3) {
		if (FIXNUM_P(argv[2]))
			addr = FIX2LONG(argv[2]);
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		addr = 1;

	if (!pink_trace_cont(pid, sig, (char *)addr))
		rb_sys_fail("pink_trace_cont()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.resume
 * call-seq: PinkTrace::Trace.resume(pid, [sig=0]) => nil
 *
 * Resumes the stopped child process. This is equivalent to
 * PinkTrace::Trace.cont(pid, sig, 1)
 *
 * If +sig+ argument is non-zero and not SIGSTOP, it is interpreted as the
 * signal to be delivered to the child; otherwise, no signal is delivered.
 * Thus, for example, the parent can control whether a signal sent to the child
 * is delivered or not.
 */
static VALUE
pinkrb_trace_resume(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	long sig;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc > 1) {
		if (FIXNUM_P(argv[1]))
			sig = FIX2LONG(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		sig = 0;

	if (!pink_trace_resume(pid, sig))
		rb_sys_fail("pink_trace_resume()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.kill
 * call-seq: PinkTrace::Trace.kill(pid) => nil
 *
 * Kills the traced child process with SIGKILL.
 */
static VALUE
pinkrb_trace_kill(PINK_UNUSED VALUE mod, VALUE pidv)
{
	pid_t pid;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (!pink_trace_kill(pid))
		rb_sys_fail("pink_trace_kill()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.singlestep
 * call-seq: PinkTrace::Trace.singlestep(pid, [sig=0]) => nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * execution of a single instruction.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 */
static VALUE
pinkrb_trace_singlestep(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	long sig;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			sig = FIX2LONG(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		sig = 0;

	if (!pink_trace_singlestep(pid, sig))
		rb_sys_fail("pink_trace_singlestep()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.syscall
 * call-seq: PinkTrace::Trace.syscall(pid, [sig=0]) => nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * the entry or exit of the next system call.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 */
static VALUE
pinkrb_trace_syscall(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	long sig;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			sig = FIX2LONG(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		sig = 0;

	if (!pink_trace_syscall(pid, sig))
		rb_sys_fail("pink_trace_syscall()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.syscall_entry
 * call-seq: PinkTrace::Trace.syscall_entry(pid, [sig=0]) => nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * the entry of next system call.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 *
 * Availability: FreeBSD
 */
#if !defined(PINKTRACE_FREEBSD)
PINK_NORETURN
#endif
static VALUE
pinkrb_trace_syscall_entry(
#if !defined(PINKTRACE_FREEBSD)
	PINK_UNUSED int argc, PINK_UNUSED VALUE *argv,
#else
	int argc, VALUE *argv,
#endif
	PINK_UNUSED VALUE mod)
{
#if defined(PINKTRACE_FREEBSD)
	pid_t pid;
	long sig;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			sig = FIX2LONG(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		sig = 0;

	if (!pink_trace_syscall_entry(pid, sig))
		rb_sys_fail("pink_trace_syscall_entry()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_FREEBSD) */
}

/*
 * Document-method: PinkTrace::Trace.syscall_exit
 * call-seq: PinkTrace::Trace.syscall_exit(pid, [sig=0]) => nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * the exit of next system call.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 *
 * Availability: FreeBSD
 */
#if !defined(PINKTRACE_FREEBSD)
PINK_NORETURN
#endif
static VALUE
pinkrb_trace_syscall_exit(
#if !defined(PINKTRACE_FREEBSD)
	PINK_UNUSED int argc, PINK_UNUSED VALUE *argv,
#else
	int argc, VALUE *argv,
#endif
	PINK_UNUSED VALUE mod)
{
#if defined(PINKTRACE_FREEBSD)
	pid_t pid;
	long sig;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			sig = FIX2LONG(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		sig = 0;

	if (!pink_trace_syscall_exit(pid, sig))
		rb_sys_fail("pink_trace_syscall_exit()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_FREEBSD) */
}

/*
 * Document-method: PinkTrace::Trace.geteventmsg
 * call-seq: PinkTrace::Trace.geteventmsg(pid) => fixnum
 *
 * Returns a message (as a <tt>fixnum</tt>) about the trace event that just
 * happened, For *EXIT* event this is the child's exit status. For *FORK*,
 * *VFORK*, *CLONE* and *VFORK_DONE* events this is the process ID of the new
 * process.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_trace_geteventmsg(PINK_UNUSED VALUE mod,
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED
#endif
	VALUE pidv)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	unsigned long data;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "Process ID is not a Fixnum");

	if (!pink_trace_geteventmsg(pid, &data))
		rb_sys_fail("pink_trace_geteventmsg()");

	return ULONG2NUM(data);
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-method: PinkTrace::Trace.setup
 * call-seq: PinkTrace::Trace.setup(pid, [options=PinkTrace::Trace::OPTION_SYSGOOD]) => nil
 *
 * Sets the tracing options.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_trace_setup(
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED int argc, PINK_UNUSED VALUE *argv,
#else
	int argc, VALUE *argv,
#endif
	PINK_UNUSED VALUE mod)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	int opts;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			opts = FIX2INT(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		opts = PINK_TRACE_OPTION_SYSGOOD;

	if (!pink_trace_setup(pid, opts))
		rb_sys_fail("pink_trace_setup()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-method: PinkTrace::Trace.attach
 * call-seq: PinkTrace::Trace.attach(pid) => nil
 *
 * Attaches to the process specified in pid, making it a traced "child" of the
 * calling process; the behaviour of the child is as if it had done a
 * PinkTrace::Trace.me. The child is sent a SIGSTOP, but will not necessarily have
 * stopped by the completion of this call; use Process.waitpid to wait for the
 * child to stop.
 */
static VALUE
pinkrb_trace_attach(PINK_UNUSED VALUE mod, VALUE pidv)
{
	pid_t pid;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "Process ID is not a Fixnum");

	if (!pink_trace_attach(pid))
		rb_sys_fail("pink_trace_attach()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.detach
 * call-seq: PinkTrace::Trace.detach(pid, [sig=0]) => nil
 *
 * Restarts the stopped child as for PinkTrace::Trace.cont, but first detaches
 * from the process, undoing the reparenting effect of PinkTrace::Trace.attach.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 */
static VALUE
pinkrb_trace_detach(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	long sig;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			sig = FIX2LONG(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		sig = 0;

	if (!pink_trace_detach(pid, sig))
		rb_sys_fail("pink_trace_detach()");

	return Qnil;
}

/*
 * Document-class: PinkTrace::Event
 *
 * This class defines constants and functions for event decisions.
 *
 * == Constants
 *
 * Note: The constants below are only available on Linux.
 *
 * - PinkTrace::Event::EVENT_STOP
 *
 *   The traced child has received a SIGSTOP.
 *
 * - PinkTrace::Event::EVENT_SYSCALL
 *
 *   The traced child is entering or exiting a system call.
 *
 * - PinkTrace::Event::EVENT_FORK
 *
 *   The traced child called fork(2).
 *
 * - PinkTrace::Event::EVENT_VFORK
 *
 *   The traced child called vfork(2).
 *
 * - PinkTrace::Event::EVENT_CLONE
 *
 *   The traced child called clone(2).
 *
 * - PinkTrace::Event::EVENT_VFORK_DONE
 *
 *   The traced child is exiting a vfork(2) call.
 *
 * - PinkTrace::Event::EVENT_EXEC
 *
 *   The traced child is exiting. (ptrace way, stopped before exit)
 *
 * - PinkTrace::Event::EVENT_GENUINE
 *
 *   The traced child has received a genuine signal.
 *
 * - PinkTrace::Event::EVENT_EXIT_GENUINE
 *
 *   The traced child has exited normally.
 *
 * - PinkTrace::Event::EVENT_EXIT_SIGNAL
 *
 *   The traced child has been terminated with a signal.
 *
 */

/*
 * Document-method: PinkTrace::Event.decide
 * call-seq: PinkTrace::Event.decide([status=$?.status]) => fixnum
 *
 * Returns the last event made by child.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_event_decide(
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED int argc, PINK_UNUSED VALUE *argv,
#else
	int argc, VALUE *argv,
#endif
	PINK_UNUSED VALUE mod)
{
#if defined(PINKTRACE_LINUX)
	unsigned int event;
	int status;

	if (argc > 1)
		rb_raise(rb_eArgError, "Wrong number of arguments");
	else if (argc == 1) {
		if (FIXNUM_P(argv[0]))
			status = FIX2INT(argv[0]);
		else
			rb_raise(rb_eTypeError, "First argument is not a Fixnum");
	}
	else {
#ifdef HAVE_RB_LAST_STATUS_GET /* ruby-1.9 */
		status = FIX2INT(rb_iv_get(rb_last_status_get(), "status"));
#else /* ruby-1.8 */
		VALUE ls = rb_gv_get("$?");
		if (NIL_P(ls))
			rb_raise(rb_eTypeError, "$? is nil");
		else
			status = FIX2INT(rb_iv_get(ls, "status"));
#endif /* HAVE_RB_LAST_STATUS_GET */
	}

	event = pink_event_decide(status);
	return UINT2NUM(event);
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-class: PinkTrace::Bitness
 *
 * This class defines constants and functions about bitness.
 *
 * == Constants
 *
 * - PinkTrace::Bitness::COUNT_SUPPORTED
 *
 *   Number of supported bitnesses (eg 2 on x86_64, 1 on i386)
 *
 * - PinkTrace::Bitness::DEFAULT
 *
 *   The default bitness
 *
 * - PinkTrace::Bitness::BITNESS_32
 *
 *   32 bit mode
 *
 * - PinkTrace::Bitness::BITNESS_64
 *
 *   64 bit mode
 *
 * - PinkTrace::Bitness::BITNESS_32_SUPPORTED
 *
 *   True if 32 bit is supported, False otherwise
 *
 * - PinkTrace::Bitness::BITNESS_64_SUPPORTED
 *
 *   True if 64 bit is supported, False otherwise
 */

/*
 * Document-method: PinkTrace::Bitness.get
 * call-seq: PinkTrace::Bitness.get(pid) => fixnum
 *
 * Returns the bitness of the traced child.
 */
static VALUE
pinkrb_bitness_get(PINK_UNUSED VALUE mod, VALUE pidv)
{
	pid_t pid;
	int bit;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	bit = pink_bitness_get(pid);
	if (bit == PINK_BITNESS_UNKNOWN)
		rb_sys_fail("pink_bitness_get()");

	return INT2FIX(bit);
}

/*
 * Document-method: Pinktrace::Bitness.name
 * call-seq: PinkTrace::Bitness.name(bitness) => String
 *
 * Returns the name of the given bitness.
 */
static VALUE
pinkrb_bitness_name(PINK_UNUSED VALUE mod, VALUE bitv)
{
	pink_bitness_t bit;

	if (FIXNUM_P(bitv))
		bit = FIX2UINT(bitv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	return rb_str_new2(pink_bitness_name(bit));
}

/*
 * Document-class: PinkTrace::Syscall
 *
 * This class defines utilities useful when tracing processes.
 *
 * == Constants
 *
 * - PinkTrace::Syscall::INVALID
 *
 *   This constant is an invalid system call number. You may use this constant
 *   as an argument to PinkTrace::Syscall.set_no to deny a system call from
 *   execution.
 *
 * - PinkTrace::Syscall::MAX_INDEX
 *
 *   The index arguments of system call functions must be smaller than this
 *   constant.
 */

/*
 * Document-method: PinkTrace::Syscall.name
 * call-seq: PinkTrace::Syscall.name(scno, [bitness=PinkTrace::Bitness::DEFAULT]) => String or nil
 *
 * Return the name of the given system call.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_name_syscall(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	unsigned bit;
	long scno;
	const char *scname;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		scno = FIX2LONG(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1])) {
			bit = FIX2UINT(argv[1]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	scname = pink_name_syscall(scno, bit);
	return scname ? rb_str_new2(scname) : Qnil;
}

/*
 * Document-method: PinkTrace::Syscall.lookup
 * call-seq: PinkTrace::Syscall.lookup(name, [bitness=PinkTrace::Bitness::DEFAULT]) => fixnum
 *
 * Look up the given system call name.
 * Returns -1 if the lookup fails.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_name_lookup(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	unsigned bit;
	const char *name;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

#if !defined(RSTRING_PTR)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_PTR) */

	SafeStringValue(argv[0]);
	name = RSTRING_PTR(argv[0]);

	if (argc == 2) {
		if (FIXNUM_P(argv[1])) {
			bit = FIX2UINT(argv[1]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	return LONG2NUM(pink_name_lookup(name, bit));
}

/*
 * Document-method: PinkTrace::Syscall.get_no
 * call-seq: PinkTrace::Syscall.get_no(pid, [bitness=PinkTrace::Bitness::DEFAULT]) => fixnum
 *
 * Returns the last system call number called by the traced child.
 */
static VALUE
pinkrb_util_get_syscall(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	unsigned bit;
	long scno;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2LONG(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1])) {
			bit = FIX2UINT(argv[1]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (!pink_util_get_syscall(pid, bit, &scno))
		rb_sys_fail("pink_util_get_syscall()");

	return LONG2FIX(scno);
}

/*
 * Document-method: PinkTrace::Syscall.set_no
 * call-seq: PinkTrace::Syscall.set_no(pid, scno, [bitness=PinkTrace::Bitness::DEFAULT]) => nil
 *
 * Sets the system call number for the traced child.
 */
static VALUE
pinkrb_util_set_syscall(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	unsigned bit;
	long scno;

	if (argc < 2 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1]))
		scno = FIX2LONG(argv[1]);
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc == 3) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (!pink_util_set_syscall(pid, bit, scno))
		rb_sys_fail("pink_util_set_syscall()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Syscall.get_ret
 * call-seq: PinkTrace::Syscall.get_ret(pid) => fixnum
 *
 * Returns the return value of the last system call called by the traced child.
 */
static VALUE
pinkrb_util_get_return(PINK_UNUSED VALUE mod, VALUE pidv)
{
	pid_t pid;
	long ret;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (!pink_util_get_return(pid, &ret))
		rb_sys_fail("pink_util_get_return()");

	return LONG2FIX(ret);
}

/*
 * Document-method: PinkTrace::Syscall.set_ret
 * call-seq: PinkTrace::Syscall.set_ret(pid, ret) => nil
 *
 * Set the return value of the system call for the traced child.
 */
static VALUE
pinkrb_util_set_return(PINK_UNUSED VALUE mod, VALUE pidv, VALUE retv)
{
	pid_t pid;
	long ret;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(retv))
		ret = FIX2LONG(retv);
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (!pink_util_set_return(pid, ret))
		rb_sys_fail("pink_util_set_return()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Syscall.get_arg
 * call-seq: PinkTrace::Syscall.get_arg(pid, index, [bitness=PinkTrace::Bitness::Default]) => fixnum
 *
 * Returns the system call argument at the given index for the traced child.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::Syscall::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_util_get_arg(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	long arg;

	if (argc < 2 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		check_index(ind);
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (!pink_util_get_arg(pid, bit, ind, &arg))
		rb_sys_fail("pink_util_get_arg()");

	return LONG2FIX(arg);
}

/*
 * Document-class: PinkTrace::String
 *
 * This class contains functions to decode/encode string arguments.
 */

/*
 * Document-method: PinkTrace::String.decode
 * call-seq: PinkTrace::String.decode(pid, index, [[maxlen=-1], [bitness=PinkTrace::Bitness::DEFAULT]]) => String
 *
 * This function decodes the string at the argument of the given index. If
 * +maxlen+ is smaller than zero, which is the default, pinktrace tries to
 * determine the length of the string itself.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::Syscall::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_string(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	int maxlen;
	char *str;
	VALUE ret;

	if (argc < 2 || argc > 4)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2UINT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		check_index(ind);
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc > 2) {
		if (FIXNUM_P(argv[2]))
			maxlen = FIX2INT(argv[2]);
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		maxlen = -1;

	if (argc > 3) {
		if (FIXNUM_P(argv[3])) {
			bit = FIX2UINT(argv[3]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Fourth argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (maxlen < 0) {
		/* Use pink_decode_string_persistent() */
		str = pink_decode_string_persistent(pid, bit, ind);
		if (!str)
			rb_sys_fail("pink_decode_string_persistent()");

		ret = rb_str_new2(str);
		free(str);
		return ret;
	}

	/* Use pink_decode_string() */
	str = ALLOC_N(char, maxlen);
	if (!pink_decode_string(pid, bit, ind, str, maxlen))
		rb_sys_fail("pink_decode_string()");

	ret = rb_str_new2(str);
	if (str)
		free(str);
	return ret;
}

/*
 * Document-method: PinkTrace::String.encode
 * call-seq: PinkTrace::String.encode(pid, index, str, [bitness=PinkTrace::Bitness::DEFAULT]) => nil
 *
 * Encode a string into the argument of the given index safely.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::Syscall::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_encode_string_safe(
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED int argc, PINK_UNUSED VALUE *argv,
#else
	int argc, VALUE *argv,
#endif
	PINK_UNUSED VALUE mod)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	unsigned bit, ind;
	size_t len;
	char *src;

	if (argc < 3 || argc > 4)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2UINT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		check_index(ind);
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

#if !defined(RSTRING_LEN)
#define RSTRING_LEN(v) (RSTRING((v))->len)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_LEN) */

	SafeStringValue(argv[2]);
	src = RSTRING_PTR(argv[2]);
	len = RSTRING_LEN(argv[2]);

	if (argc > 3) {
		if (FIXNUM_P(argv[3])) {
			bit = FIX2UINT(argv[3]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Fourth argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (!pink_encode_simple_safe(pid, bit, ind, src, ++len))
		rb_sys_fail("pink_encode_simple_safe()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-method: PinkTrace::String.encode!
 * call-seq: PinkTrace::String.encode!(pid, index, str, [bitness=PinkTrace::Bitness::DEFAULT]) => nil
 *
 * Encode a string into the argument of the given index.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::Syscall::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_encode_string(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	size_t len;
	char *src;

	if (argc < 3 || argc > 4)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2UINT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		check_index(ind);
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

#if !defined(RSTRING_LEN)
#define RSTRING_LEN(v) (RSTRING((v))->len)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_LEN) */

	SafeStringValue(argv[2]);
	src = RSTRING_PTR(argv[2]);
	len = RSTRING_LEN(argv[2]);

	if (argc > 3) {
		if (FIXNUM_P(argv[3])) {
			bit = FIX2UINT(argv[3]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Fourth argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (!pink_encode_simple(pid, bit, ind, src, ++len))
		rb_sys_fail("pink_encode_simple()");

	return Qnil;
}

/*
 * Document-class: PinkTrace::Socket
 *
 * This class includes functions for decoding socket calls.
 */

/*
 * Document-method: PinkTrace::Socket.has_socketcall?
 * call-seq: PinkTrace::Socket.has_socketcall?([bitness=PinkTrace::Bitness::DEFAULT]) => true or false
 *
 * Returns true if the socket calls - like connect, bind, sendto etc. - are
 * implemented as subcalls of the socketcall(2) system call, false otherwise.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_has_socketcall(PINK_UNUSED VALUE mod,
#if !defined(PINKTRACE_LINUX)
		PINK_UNUSED
#endif
		int argc,
#if !defined(PINKTRACE_LINUX)
		PINK_UNUSED
#endif
		VALUE *argv)
{
#if defined(PINKTRACE_LINUX)
	unsigned bit;

	if (argc > 1)
		rb_raise(rb_eArgError, "Wrong number of arguments");
	else if (argc > 0) {
		if (FIXNUM_P(argv[0])) {
			bit = FIX2UINT(argv[0]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "First argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	return pink_has_socketcall(bit) ? Qtrue : Qfalse;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-method: PinkTrace::Socket.name
 * call-seq: PinkTrace::Socket.name(subcall) => String or nil
 *
 * Returns a string representation of the socket subcall.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_name_socket_subcall(PINK_UNUSED VALUE mod,
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED
#endif
	VALUE subcallv)
{
#if defined(PINKTRACE_LINUX)
	unsigned subcall;
	const char *subname;

	if (FIXNUM_P(subcallv))
		subcall = FIX2UINT(subcallv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	subname = pink_name_socket_subcall(subcall);

	return subname ? rb_str_new2(subname) : Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-method: PinkTrace::Socket.decode_call
 * call-seq: PinkTrace::Socket.decode_call(pid, [bitness=PinkTrace::Bitness::DEFAULT]) => fixnum
 *
 * Returns the decoded socket call.
 *
 * Note: This function decodes the socketcall(2) system call on some
 * architectures. On others it's equivalent to PinkTrace::Syscall.get_no
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_decode_socket_call(
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED int argc, PINK_UNUSED VALUE *argv,
#else
	int argc, VALUE *argv,
#endif
	PINK_UNUSED VALUE mod)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	unsigned bit;
	long subcall;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc > 1) {
		if (FIXNUM_P(argv[1])) {
			bit = FIX2UINT(argv[1]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (!pink_decode_socket_call(pid, bit, &subcall))
		rb_sys_fail("pink_decode_socket_call()");

	return LONG2NUM(subcall);
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-method: PinkTrace::Socket.decode_fd
 * call-seq: PinkTrace::Socket.decode_fd(pid, [[index=0], [bitness=PinkTrace::Bitness::DEFAULT]]) => fixnum
 *
 * Returns the socket file descriptor.
 *
 * Note: This function decodes the socketcall(2) system call on some
 * architectures.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::Syscall::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 *
 * Availability: Linux
 */
#if !defined(PINKTRACE_LINUX)
PINK_NORETURN
#endif
static VALUE
pinkrb_decode_socket_fd(
#if !defined(PINKTRACE_LINUX)
	PINK_UNUSED int argc, PINK_UNUSED VALUE *argv,
#else
	int argc, VALUE *argv,
#endif
	PINK_UNUSED VALUE mod)
{
#if defined(PINKTRACE_LINUX)
	pid_t pid;
	unsigned bit, ind;
	long fd;

	if (argc < 1 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc > 1) {
		if (FIXNUM_P(argv[1])) {
			ind = FIX2UINT(argv[1]);
			check_index(ind);
		}
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		ind = 0;

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	if (!pink_decode_socket_fd(pid, bit, ind, &fd))
		rb_sys_fail("pink_decode_socket_fd()");

	return LONG2NUM(fd);
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_LINUX) */
}

/*
 * Document-class: PinkTrace::Socket::Address
 *
 * This class represents a decoded socket address.
 */

/*
 * Document-method: PinkTrace::Socket.decode_address
 * call-seq: PinkTrace::Socket.decode_address(pid, index, [bitness=PinkTrace::Bitness::DEFAULT]) => addr or nil
 *
 * Decodes the socket address at the given index.
 * If the system call's address argument was NULL, this function returns +nil+.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::Syscall::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_socket_address(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	pink_socket_address_t *addr;
	VALUE addrObj;

	if (argc < 2 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		check_index(ind);
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	addrObj = Data_Make_Struct(pinkrb_cAddress, pink_socket_address_t, NULL, free, addr);

	if (!pink_decode_socket_address(pid, bit, ind, NULL, addr))
		rb_sys_fail("pink_decode_socket_address()");

	return addrObj;
}

/*
 * Document-method: PinkTrace::Socket.decode_address_fd
 * call-seq: PinkTrace::Socket.decode_address_fd(pid, index, [bitness=PinkTrace::Bitness::DEFAULT]) => addr|nil, fd
 *
 * Decodes the socket address at the given index and the file descriptor at index 0.
 * If the system call's address argument was NULL this function returns +nil+
 * and the file descriptor.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::Syscall::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_socket_address_fd(int argc, VALUE *argv, PINK_UNUSED VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	long fd;
	pink_socket_address_t *addr;
	VALUE addrObj;

	if (argc < 2 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		check_index(ind);
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
			check_bitness(bit);
		}
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_BITNESS_DEFAULT;

	addrObj = Data_Make_Struct(pinkrb_cAddress, pink_socket_address_t, NULL, free, addr);

	if (!pink_decode_socket_address(pid, bit, ind, &fd, addr))
		rb_sys_fail("pink_decode_socket_address()");

	return rb_assoc_new(addrObj, LONG2NUM(fd));
}

/*
 * Document-method: family
 * call-seq: addr.family => fixnum
 *
 * Returns the family of the address.
 */
static VALUE
pinkrb_Address_family(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);

	return INT2FIX(addr->family);
}

/*
 * Document-method: to_s
 * call-seq: addr.to_s => String
 *
 * Returns the string representation of the address.
 * For UNIX addresses this is the path, for INET{,6} addresses this is the IP.
 */
static VALUE
pinkrb_Address_to_s(VALUE self)
{
	char *ip;
	pink_socket_address_t *addr;
	VALUE ret;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	switch (addr->family) {
	case -1:
		return rb_str_new2("NULL");
	case AF_UNIX:
		if (IS_ABSTRACT(addr)) {
			addr->u.sa_un.sun_path[0] = '@';
			ret = rb_str_new2(addr->u.sa_un.sun_path);
			addr->u.sa_un.sun_path[0] = '\0';
			return ret;
		}
		return rb_str_new2(addr->u.sa_un.sun_path);
	case AF_INET:
		ip = ALLOC_N(char, INET_ADDRSTRLEN);
		inet_ntop(AF_INET, &addr->u.sa_in.sin_addr, ip, INET_ADDRSTRLEN);
		ret = rb_str_new2(ip);
		free(ip);
		return ret;
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
		ip = ALLOC_N(char, INET6_ADDRSTRLEN);
		inet_ntop(AF_INET6, &addr->u.sa6.sin6_addr, ip, INET6_ADDRSTRLEN);
		ret = rb_str_new2(ip);
		free(ip);
		return ret;
#endif /* PINKTRACE_HAVE_IPV6 */
	default:
		return rb_str_new2("UNKNOWN");
	}
}

/*
 * Document-method: unix?
 * call-seq: addr.unix? => true or false
 *
 * Returns true if the address is of family +AF_UNIX+.
 */
static VALUE
pinkrb_Address_is_unix(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	return (addr->family == AF_UNIX) ? Qtrue : Qfalse;
}

/*
 * Document-method: abstract?
 * call-seq: addr.abstract? => true or false
 *
 * Returns true if the address is an abstract UNIX socket address.
 */
static VALUE
pinkrb_Address_is_abstract(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family == AF_UNIX && IS_ABSTRACT(addr))
		return Qtrue;
	return Qfalse;
}

void
Init_PinkTrace(void)
{
	VALUE mod;
	VALUE bitness_mod;
	VALUE event_mod;
	VALUE string_mod;
	VALUE socket_mod;
	VALUE syscall_mod;
	VALUE trace_mod;

	/* PinkTrace module */
	mod = rb_define_module("PinkTrace");
	pinkrb_eBitnessError = rb_define_class_under(mod, "BitnessError", rb_eStandardError);
	pinkrb_eIndexError = rb_define_class_under(mod, "IndexError", rb_eIndexError);

	/* Global Constants */
#if PINKTRACE_HAVE_IPV6
	rb_define_const(mod, "HAVE_IPV6", Qtrue);
#else
	rb_define_const(mod, "HAVE_IPV6", Qfalse);
#endif /* PINKTRACE_HAVE_IPV6 */
	/* about.h */
	rb_define_const(mod, "PACKAGE", rb_str_new2(PINKTRACE_PACKAGE));
	rb_define_const(mod, "VERSION", INT2FIX(PINKTRACE_VERSION));
	rb_define_const(mod, "VERSION_MAJOR", INT2FIX(PINKTRACE_VERSION_MAJOR));
	rb_define_const(mod, "VERSION_MINOR", INT2FIX(PINKTRACE_VERSION_MINOR));
	rb_define_const(mod, "VERSION_MICRO", INT2FIX(PINKTRACE_VERSION_MICRO));
	rb_define_const(mod, "VERSION_SUFFIX", rb_str_new2(PINKTRACE_VERSION_SUFFIX));
	rb_define_const(mod, "GIT_HEAD", rb_str_new2(PINKTRACE_GIT_HEAD));
	rb_define_const(mod, "PC_SLOT", rb_str_new2(PINKTRACE_PC_SLOT));

	/* trace.h */
	trace_mod = rb_define_module_under(mod, "Trace");
#if defined(PINKTRACE_LINUX)
	rb_define_const(trace_mod, "OPTION_SYSGOOD", INT2FIX(PINK_TRACE_OPTION_SYSGOOD));
	rb_define_const(trace_mod, "OPTION_FORK", INT2FIX(PINK_TRACE_OPTION_FORK));
	rb_define_const(trace_mod, "OPTION_VFORK", INT2FIX(PINK_TRACE_OPTION_VFORK));
	rb_define_const(trace_mod, "OPTION_CLONE", INT2FIX(PINK_TRACE_OPTION_CLONE));
	rb_define_const(trace_mod, "OPTION_EXEC", INT2FIX(PINK_TRACE_OPTION_EXEC));
	rb_define_const(trace_mod, "OPTION_VFORK_DONE", INT2FIX(PINK_TRACE_OPTION_VFORK_DONE));
	rb_define_const(trace_mod, "OPTION_EXIT", INT2FIX(PINK_TRACE_OPTION_EXIT));
	rb_define_const(trace_mod, "OPTION_ALL", INT2FIX(PINK_TRACE_OPTION_ALL));
#endif /* defined(PINKTRACE_LINUX) */
	rb_define_module_function(trace_mod, "me", pinkrb_trace_me, 0);
	rb_define_module_function(trace_mod, "cont", pinkrb_trace_cont, -1);
	rb_define_module_function(trace_mod, "resume", pinkrb_trace_resume, -1);
	rb_define_module_function(trace_mod, "kill", pinkrb_trace_kill, 1);
	rb_define_module_function(trace_mod, "singlestep", pinkrb_trace_singlestep, -1);
	rb_define_module_function(trace_mod, "syscall", pinkrb_trace_syscall, -1);
	rb_define_module_function(trace_mod, "syscall_entry", pinkrb_trace_syscall_entry, -1);
	rb_define_module_function(trace_mod, "syscall_exit", pinkrb_trace_syscall_exit, -1);
	rb_define_module_function(trace_mod, "geteventmsg", pinkrb_trace_geteventmsg, 1);
	rb_define_module_function(trace_mod, "setup", pinkrb_trace_setup, -1);
	rb_define_module_function(trace_mod, "attach", pinkrb_trace_attach, 1);
	rb_define_module_function(trace_mod, "detach", pinkrb_trace_detach, -1);

	/* event.h */
	event_mod = rb_define_module_under(mod, "Event");
#if defined(PINKTRACE_LINUX)
	rb_define_const(event_mod, "EVENT_STOP", INT2FIX(PINK_EVENT_STOP));
	rb_define_const(event_mod, "EVENT_SYSCALL", INT2FIX(PINK_EVENT_SYSCALL));
	rb_define_const(event_mod, "EVENT_FORK", INT2FIX(PINK_EVENT_FORK));
	rb_define_const(event_mod, "EVENT_VFORK", INT2FIX(PINK_EVENT_VFORK));
	rb_define_const(event_mod, "EVENT_CLONE", INT2FIX(PINK_EVENT_CLONE));
	rb_define_const(event_mod, "EVENT_VFORK_DONE", INT2FIX(PINK_EVENT_VFORK_DONE));
	rb_define_const(event_mod, "EVENT_EXEC", INT2FIX(PINK_EVENT_EXEC));
	rb_define_const(event_mod, "EVENT_EXIT", INT2FIX(PINK_EVENT_EXIT));
	rb_define_const(event_mod, "EVENT_GENUINE", INT2FIX(PINK_EVENT_GENUINE));
	rb_define_const(event_mod, "EVENT_EXIT_GENUINE", INT2FIX(PINK_EVENT_EXIT_GENUINE));
	rb_define_const(event_mod, "EVENT_EXIT_SIGNAL", INT2FIX(PINK_EVENT_EXIT_SIGNAL));
	rb_define_const(event_mod, "EVENT_UNKNOWN", INT2FIX(PINK_EVENT_UNKNOWN));
#endif /* defined(PINKTRACE_LINUX) */
	rb_define_module_function(event_mod, "decide", pinkrb_event_decide, -1);

	/* bitness.h */
	bitness_mod = rb_define_module_under(mod, "Bitness");
	rb_define_const(bitness_mod, "COUNT_SUPPORTED", INT2FIX(PINKTRACE_BITNESS_COUNT_SUPPORTED));
	rb_define_const(bitness_mod, "DEFAULT", INT2FIX(PINKTRACE_BITNESS_DEFAULT));
	rb_define_const(bitness_mod, "BITNESS_32", INT2FIX(PINK_BITNESS_32));
	rb_define_const(bitness_mod, "BITNESS_64", INT2FIX(PINK_BITNESS_64));
#if PINKTRACE_BITNESS_32_SUPPORTED
	rb_define_const(bitness_mod, "BITNESS_32_SUPPORTED", Qtrue);
#else
	rb_define_const(bitness_mod, "BITNESS_32_SUPPORTED", Qfalse);
#endif /* PINKTRACE_BITNESS_32_SUPPORTED */
#if PINKTRACE_BITNESS_64_SUPPORTED
	rb_define_const(bitness_mod, "BITNESS_64_SUPPORTED", Qtrue);
#else
	rb_define_const(bitness_mod, "BITNESS_64_SUPPORTED", Qfalse);
#endif /* PINKTRACE_BITNESS_64_SUPPORTED */
	rb_define_module_function(bitness_mod, "get", pinkrb_bitness_get, 1);
	rb_define_module_function(bitness_mod, "name", pinkrb_bitness_name, 1);

	/* util.h && name.h */
	syscall_mod = rb_define_module_under(mod, "Syscall");
	rb_define_const(syscall_mod, "INVALID", INT2FIX(PINKTRACE_INVALID_SYSCALL));
	rb_define_const(syscall_mod, "MAX_INDEX", INT2FIX(PINK_MAX_INDEX));
	rb_define_module_function(syscall_mod, "name", pinkrb_name_syscall, -1);
	rb_define_module_function(syscall_mod, "lookup", pinkrb_name_lookup, -1);
	rb_define_module_function(syscall_mod, "get_no", pinkrb_util_get_syscall, -1);
	rb_define_module_function(syscall_mod, "set_no", pinkrb_util_set_syscall, -1);
	rb_define_module_function(syscall_mod, "get_ret", pinkrb_util_get_return, 1);
	rb_define_module_function(syscall_mod, "set_ret", pinkrb_util_set_return, 2);
	rb_define_module_function(syscall_mod, "get_arg", pinkrb_util_get_arg, -1);

	/* decode.h && encode.h (only string {en,de}coding) */
	string_mod = rb_define_module_under(mod, "String");
	rb_define_module_function(string_mod, "decode", pinkrb_decode_string, -1);
	rb_define_module_function(string_mod, "encode", pinkrb_encode_string_safe, -1);
	rb_define_module_function(string_mod, "encode!", pinkrb_encode_string, -1);

	/* decode.h && socket.h */
	socket_mod = rb_define_module_under(mod, "Socket");
	rb_define_module_function(socket_mod, "has_socketcall?", pinkrb_has_socketcall, -1);
	rb_define_module_function(socket_mod, "name", pinkrb_name_socket_subcall, 1);
	rb_define_module_function(socket_mod, "decode_call", pinkrb_decode_socket_call, -1);
	rb_define_module_function(socket_mod, "decode_fd", pinkrb_decode_socket_fd, -1);

	/* Address Objects */
	pinkrb_cAddress = rb_define_class_under(socket_mod, "Address", rb_cObject);

	/* The address objects are only returned by PinkTrace::Socket.decode_address;
	 * thus we don't need an initialize method. */
	rb_undef_method(pinkrb_cAddress, "initialize");

	/* Address methods */
	rb_define_method(pinkrb_cAddress, "family", pinkrb_Address_family, 0);
	rb_define_method(pinkrb_cAddress, "to_s", pinkrb_Address_to_s, 0);
	rb_define_method(pinkrb_cAddress, "unix?", pinkrb_Address_is_unix, 0);
	rb_define_method(pinkrb_cAddress, "abstract?", pinkrb_Address_is_abstract, 0);

	rb_define_module_function(socket_mod, "decode_address", pinkrb_decode_socket_address, -1);
	rb_define_module_function(socket_mod, "decode_address_fd", pinkrb_decode_socket_address_fd, -1);
}
