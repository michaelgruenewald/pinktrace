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
#endif /* !PIDT2NUM */

void
Init_PinkTrace(void);

static VALUE pinkrb_eAddressError;
static VALUE pinkrb_eBitnessError;
static VALUE pinkrb_eEventError;
static VALUE pinkrb_eIndexError;

static VALUE pinkrb_cAddress;
static VALUE pinkrb_cUNIXAddress;
static VALUE pinkrb_cINETAddress;
#if PINKTRACE_HAVE_IPV6
static VALUE pinkrb_cINET6Address;
#endif

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
 * - PinkTrace::SysCall
 * - PinkTrace::String
 * - PinkTrace::Socket
 *
 * == Constants
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
 * - PinkTrace::HAVE_IPV6
 *
 *   This constant can be used to figure out if IPV6 support was compiled in.
 *   +false+ if IPV6 support isn't available, +true+ otherwise.
 *
 * == Exceptions
 *
 * - PinkTrace::AddressError
 * - PinkTrace::BitnessError
 * - PinkTrace::EventError
 * - PinkTrace::IndexError
 */

/*
 * Document-class: PinkTrace::AddressError
 *
 * Raised when the address family of a system call is unsupported.
 *
 * Currently three families are supported:
 *
 * - AF_UNIX
 * - AF_INET
 * - AF_INET6
 */

/*
 * Document-class: PinkTrace::BitnessError
 *
 * Raised when the given bitness argument is either unsupported or undefined.
 */

/*
 * Document-class: PinkTrace::EventError
 *
 * Raised when PinkTrace::Event.decide can't decide an event.
 */

/*
 * Document-class: PinkTrace::IndexError
 *
 * Raised when an index argument is not smaller than PinkTrace::MAX_INDEX.
 */

/*
 * Document-class: PinkTrace::Trace
 *
 * This class includes thin wrappers around the <tt>ptrace()</tt> system call.
 *
 * == Constants
 *
 * - PinkTrace::Trace::SYSGOOD
 *
 *   This constant represents the trace option SYSGOOD.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   when delivering syscall traps, bit 7 is set in signal number (i.e.,
 *   deliver (SIGTRAP | 0x80) This makes it easy for the tracer to tell the
 *   difference between normal traps and those caused by a sycall. This
 *   option may not work on all architectures.
 *
 * - PinkTrace::Trace::FORK
 *
 *   This constant represents the trace option FORK.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next fork(2) call with (SIGTRAP | PTRACE_EVENT_FORK << 8)
 *   and automatically start tracing the newly forked process, which will start with
 *   a SIGSTOP. The PID for the new process can be retrieved with PinkTrace::Trace.geteventmsg.
 *
 * - PinkTrace::Trace::VFORK
 *
 *   This constant represents the trace option VFORK.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next vfork(2) call with (SIGTRAP | PTRACE_EVENT_VFORK << 8)
 *   and automatically start tracing the newly vforked process, which will start with
 *   a SIGSTOP. The PID for the new process can be retrieved with PinkTrace::Trace.geteventmsg.
 *
 * - PinkTrace::Trace::CLONE
 *
 *   This constant represnets the trace option CLONE.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next clone(2) call with (SIGTRAP | PTRACE_EVENT_CLONE << 8)
 *   and automatically start tracing the newly cloned process, which will start with
 *   a SIGSTOP. The PID for the new process can be retrieved with PinkTrace::Trace.geteventmsg.
 *
 * - PinkTrace::Trace::EXEC
 *
 *   This constant represents the trace option EXEC.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the next execve(2) call with (SIGTRAP | PTRACE_EVENT_EXEC << 8).
 *
 * - PinkTrace::Trace::VFORK_DONE
 *
 *   This constant represents the trace option VFORK_DONE.
 *   If this flag is set in the options argument of PinkTrace::Trace.setup,
 *   stop the child at the completion of the next vfork(2) call with
 *   (SIGTRAP | PTRACE_EVENT_VFORK_DONE << 8).
 *
 * - PinkTrace::Trace::EXIT
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
 * - PinkTrace::Trace::ALL
 *
 *   This constant represents all option flags bitwise OR'ed together.
 *
 * == Exceptions
 *
 * - Errno::EBUSY
 *
 *   (i386 only) There was an error with allocating or freeing a debug register.
 *
 * - Errno::EFAULT
 *
 *   There  was  an  attempt  to read from or write to an invalid area in
 *   the parent's or child's memory, probably because the area wasn't mapped
 *   or accessible. Unfortunately, under Linux, different variations of
 *   this fault will return Errno::EIO or Errno::EFAULT more or less arbitrarily.
 *
 * - Errno::EINVAL
 *
 *   An attempt was made to set an invalid option.
 *
 * - Errno::EIO
 *
 *   Request is invalid, or an attempt was made to read from or write to an
 *   invalid area in the parent's or child's memory, or there was a word-alignment
 *   violation, or an invalid signal was specified during a restart request.
 *
 * - Errno::EPERM
 *
 *   The  specified  process  cannot be traced.  This could be because the
 *   parent has insufficient privileges (the required capability is CAP_SYS_PTRACE);
 *   unprivileged processes  cannot  trace processes that  they  cannot  send
 *   signals to or those running set-user-ID/set-group-ID programs,
 *   for obvious reasons. Alternatively, the process may already be being traced,
 *   or be init(8) (PID 1).
 *
 * - Errno::ESRCH
 *
 *   The specified process does not exist, or is not currently being traced
 *   by the caller, or is not stopped (for requests that require that).
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
pinkrb_trace_me(pink_unused VALUE mod)
{
	if (!pink_trace_me())
		rb_sys_fail("pink_trace_me()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.cont
 * call-seq: PinkTrace::Trace.cont(pid, [sig=0]) => nil
 *
 * Restarts the stopped child process.
 *
 * If +sig+ argument is non-zero and not SIGSTOP, it is interpreted as the
 * signal to be delivered to the child; otherwise, no signal is delivered.
 * Thus, for example, the parent can control whether a signal sent to the child
 * is delivered or not.
 */
static VALUE
pinkrb_trace_cont(int argc, VALUE *argv, pink_unused VALUE mod)
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

	if (!pink_trace_cont(pid, sig))
		rb_sys_fail("pink_trace_cont()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.kill
 * call-seq: PinkTrace::Trace.kill(pid) => nil
 *
 * Kills the traced child process with SIGKILL.
 */
static VALUE
pinkrb_trace_kill(pink_unused VALUE mod, VALUE pidv)
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
pinkrb_trace_singlestep(int argc, VALUE *argv, pink_unused VALUE mod)
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
pinkrb_trace_syscall(int argc, VALUE *argv, pink_unused VALUE mod)
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
 * Document-method: PinkTrace::Trace.geteventmsg
 * call-seq: PinkTrace::Trace.geteventmsg(pid) => fixnum
 *
 * Returns a message (as a <tt>fixnum</tt>) about the trace event that just
 * happened, For *EXIT* event this is the child's exit status. For *FORK*,
 * *VFORK*, *CLONE* and *VFORK_DONE* events this is the process ID of the new
 * process.
 */
static VALUE
pinkrb_trace_geteventmsg(pink_unused VALUE mod, VALUE pidv)
{
	pid_t pid;
	unsigned long data;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "Process ID is not a Fixnum");

	if (!pink_trace_geteventmsg(pid, &data))
		rb_sys_fail("pink_trace_geteventmsg()");

	return ULONG2NUM(data);
}

/*
 * Document-method: PinkTrace::Trace.setup
 * call-seq: PinkTrace::Trace.setup(pid, [options=PinkTrace::Trace::SYSGOOD]) => nil
 *
 * Sets the tracing options.
 */
static VALUE
pinkrb_trace_setup(int argc, VALUE *argv, pink_unused VALUE mod)
{
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
pinkrb_trace_attach(pink_unused VALUE mod, VALUE pidv)
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
pinkrb_trace_detach(int argc, VALUE *argv, pink_unused VALUE mod)
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
 * Document-method: PinkTrace.fork
 * call-seq: PinkTrace.fork([opts=PinkTrace::Trace::SYSGOOD]) [{ block }] => fixnum or nil
 *
 * fork(2) wrapper that sets up the child for tracing.
 *
 * Creates a subprocess. If a block is specified, that block is run in the
 * subprocess, and the subprocess terminates with a status of zero. Otherwise
 * the +fork+ call returns twice, once in the parent, returning the process ID
 * of the child, and once in the child, returning _nil_. The child stops itself
 * with a SIGSTOP and needs to be resumed with either Ptrace::Trace.cont or
 * Ptrace::Trace.singlestep or Ptrace::Trace.syscall.
 *
 * On failure, the child is either never created or killed.
 */
static VALUE
pinkrb_fork(int argc, VALUE *argv, pink_unused VALUE mod)
{
	int opts, status;
	pid_t pid;
	pink_error_t error;

	if (argc > 1)
		rb_raise(rb_eArgError, "Wrong number of arguments");
	else if (argc == 1) {
		if (FIXNUM_P(argv[0]))
			opts = FIX2INT(argv[0]);
		else
			rb_raise(rb_eTypeError, "First argument is not a Fixnum");
	}
	else
		opts = PINK_TRACE_OPTION_SYSGOOD;

	rb_secure(2);

	if ((pid = pink_fork(opts, &error)) < 0) {
		switch (error) {
		case PINK_ERROR_FORK:
			rb_sys_fail("fork(2)");
		case PINK_ERROR_WAIT:
			rb_sys_fail("wait(2)");
		case PINK_ERROR_STOP:
			rb_sys_fail("kill(2)");
		case PINK_ERROR_TRACE:
			rb_sys_fail("pink_trace_me()");
		case PINK_ERROR_TRACE_SETUP:
			rb_sys_fail("pink_trace_setup()");
		case PINK_ERROR_UNKNOWN:
		default:
			rb_sys_fail("unknown");
		}
	}
	else if (!pid) { /* child */
		rb_thread_atfork();
		if (rb_block_given_p()) {
			rb_protect(rb_yield, Qundef, &status);
			ruby_stop(status);
		}
		return Qnil;
	}
	else
		return PIDT2NUM(pid);
}

/*
 * Document-class: PinkTrace::Event
 *
 * This class defines constants and functions for event decisions.
 *
 * == Constants
 *
 * - PinkTrace::Event::STOP
 *
 *   The traced child has received a SIGSTOP.
 *
 * - PinkTrace::Event::SYSCALL
 *
 *   The traced child is entering or exiting a system call.
 *
 * - PinkTrace::Event::FORK
 *
 *   The traced child called fork(2).
 *
 * - PinkTrace::Event::VFORK
 *
 *   The traced child called vfork(2).
 *
 * - PinkTrace::Event::CLONE
 *
 *   The traced child called clone(2).
 *
 * - PinkTrace::Event::VFORK_DONE
 *
 *   The traced child is exiting a vfork(2) call.
 *
 * - PinkTrace::Event::EXEC
 *
 *   The traced child is exiting. (ptrace way, stopped before exit)
 *
 * - PinkTrace::Event::GENUINE
 *
 *   The traced child has received a genuine signal.
 *
 * - PinkTrace::Event::EXIT_GENUINE
 *
 *   The traced child has exited normally.
 *
 * - PinkTrace::Event::EXIT_SIGNAL
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
 * Note: This function raises PinkTrace::EventError if the event is unknown.
 */
static VALUE
pinkrb_event_decide(int argc, VALUE *argv, pink_unused VALUE mod)
{
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
	if (event == PINK_EVENT_UNKNOWN)
		rb_raise(pinkrb_eEventError, "Unknown event (status: %#x)", status);

	return UINT2NUM(event);
}

/*
 * Document-class: PinkTrace::Bitness
 *
 * This class defines constants and functions about bitness.
 *
 * == Constants
 *
 * - PinkTrace::Bitness::SUPPORTED
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
 */

/*
 * Document-method: PinkTrace::Bitness.get
 * call-seq: PinkTrace::Bitness.get(pid) => fixnum
 *
 * Returns the bitness of the traced child.
 */
static VALUE
pinkrb_bitness_get(pink_unused VALUE mod, VALUE pidv)
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
 * Document-class: PinkTrace::SysCall
 *
 * This class defines utilities useful when tracing processes.
 */

/*
 * Document-method: PinkTrace::SysCall.name
 * call-seq: PinkTrace::SysCall.name(scno, [bitness=PinkTrace::Bitness::DEFAULT]) => String or nil
 *
 * Return the name of the given system call.
 *
 * Note: This call depends on the generated system call names.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 *
 * You can check whether they are generated with:
 *
 *   unless PinkTrace::SysCall.name 0
 *     # Names weren't generated
 *   else
 *     # Names were generated
 *   end
 */
static VALUE
pinkrb_name_syscall(int argc, VALUE *argv, pink_unused VALUE mod)
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
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	scname = pink_name_syscall(scno, bit);
	return scname ? rb_str_new2(scname) : Qnil;
}

/*
 * Document-method: PinkTrace::SysCall.get_no
 * call-seq: PinkTrace::SysCall.get_no(pid) => fixnum
 *
 * Returns the last system call number called by the traced child.
 */
static VALUE
pinkrb_util_get_syscall(pink_unused VALUE mod, VALUE pidv)
{
	pid_t pid;
	long scno;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (!pink_util_get_syscall(pid, &scno))
		rb_sys_fail("pink_util_get_syscall()");

	return LONG2FIX(scno);
}

/*
 * Document-method: PinkTrace::SysCall.set_no
 * call-seq: PinkTrace::SysCall.set_no(pid, scno) => nil
 *
 * Sets the system call number for the traced child.
 */
static VALUE
pinkrb_util_set_syscall(pink_unused VALUE mod, VALUE pidv, VALUE scnov)
{
	pid_t pid;
	long scno;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(scnov))
		scno = FIX2LONG(scnov);
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (!pink_util_set_syscall(pid, scno))
		rb_sys_fail("pink_util_set_syscall()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::SysCall.get_ret
 * call-seq: PinkTrace::SysCall.get_ret(pid) => fixnum
 *
 * Returns the return value of the last system call called by the traced child.
 */
static VALUE
pinkrb_util_get_return(pink_unused VALUE mod, VALUE pidv)
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
 * Document-method: PinkTrace::SysCall.set_ret
 * call-seq: PinkTrace::SysCall.set_ret(pid, ret) => nil
 *
 * Set the return value of the system call for the traced child.
 */
static VALUE
pinkrb_util_set_return(pink_unused VALUE mod, VALUE pidv, VALUE retv)
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
 * Document-method: PinkTrace::SysCall.get_arg
 * call-seq: PinkTrace::SysCall.get_arg(pid, index, [bitness=PinkTrace::Bitness::Default]) => fixnum
 *
 * Returns the system call argument at the given index for the traced child.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_util_get_arg(int argc, VALUE *argv, pink_unused VALUE mod)
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
		if (ind >= PINK_MAX_INDEX)
			rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
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
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

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
 * Note: PinkTrace::EventError is raised if +index+ argument is not smaller
 * than PinkTrace::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_string(int argc, VALUE *argv, pink_unused VALUE mod)
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
		if (ind >= PINK_MAX_INDEX)
			rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
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
		else
			rb_raise(rb_eTypeError, "Fourth argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

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
 * than PinkTrace::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_encode_string_safe(int argc, VALUE *argv, pink_unused VALUE mod)
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
		if (ind >= PINK_MAX_INDEX)
			rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
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
		else
			rb_raise(rb_eTypeError, "Fourth argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	if (!pink_encode_simple_safe(pid, bit, ind, src, ++len))
		rb_sys_fail("pink_encode_simple_safe()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::String.encode!
 * call-seq: PinkTrace::String.encode!(pid, index, str, [bitness=PinkTrace::Bitness::DEFAULT]) => nil
 *
 * Encode a string into the argument of the given index.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_encode_string(int argc, VALUE *argv, pink_unused VALUE mod)
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
		if (ind >= PINK_MAX_INDEX)
			rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
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
		else
			rb_raise(rb_eTypeError, "Fourth argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	if (!pink_encode_simple(pid, bit, ind, src, ++len))
		rb_sys_fail("pink_encode_simple()");

	return Qnil;
}

/*
 * Document-class: PinkTrace::Socket
 *
 * This class includes functions for decoding socket calls.
 *
 * == Constants
 *
 * - PinkTrace::Socket::INET_ADDRSTRLEN
 *
 *   Maximum length of an INET address string
 *
 * - PinkTrace::String::INET6_ADDRSTRLEN
 *
 *   Maximum length of an INET6 address string
 */

/*
 * Document-method: PinkTrace::Socket.subcall
 * call-seq: PinkTrace::Socket.name(subcall) => String or nil
 *
 * Returns a string representation of the socket subcall.
 */
static VALUE
pinkrb_name_socket_subcall(pink_unused VALUE mod, VALUE subcallv)
{
	unsigned subcall;
	const char *subname;

	if (FIXNUM_P(subcallv))
		subcall = FIX2UINT(subcallv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	subname = pink_name_socket_subcall(subcall);

	return subname ? rb_str_new2(subname) : Qnil;
}

/*
 * Document-method: PinkTrace::Socket.decode_call
 * call-seq: PinkTrace::Socket.decode_call(pid, [bitness=PinkTrace::Bitness::DEFAULT]) => fixnum
 *
 * Returns the decoded socket call.
 *
 * Note: This function decodes the socketcall(2) system call on some
 * architectures. On others it's equivalent to PinkTrace::SysCall.get_no
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_socket_call(int argc, VALUE *argv, pink_unused VALUE mod)
{
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
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	if (!pink_decode_socket_call(pid, bit, &subcall))
		rb_sys_fail("pink_decode_socket_call()");

	return LONG2NUM(subcall);
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
 * than PinkTrace::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_socket_fd(int argc, VALUE *argv, pink_unused VALUE mod)
{
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
			if (ind >= PINK_MAX_INDEX)
				rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
		}
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		ind = 0;

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
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
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	if (!pink_decode_socket_fd(pid, bit, ind, &fd))
		rb_sys_fail("pink_decode_socket_fd()");

	return LONG2NUM(fd);
}

/*
 * Document-class: PinkTrace::Socket::Address
 *
 * This is the base class of socket addresses.
 */

/*
 * Document-class: PinkTrace::Socket::UNIXAddress
 *
 * This class represents a UNIX socket address.
 */

/*
 * Document-class: PinkTrace::Socket::INETAddress
 *
 * This class represents a INET socket address.
 */

/*
 * Document-class: PinkTrace::Socket::INET6Address
 *
 * This class represents a INET6 socket address.
 *
 * Note: This class is only available if IPV6 support was compiled in.
 * Check with PinkTrace::HAVE_IPV6.
 */

/*
 * Document-method: PinkTrace::Socket.decode_address
 * call-seq: PinkTrace::Socket.decode_address(pid, index, [bitness=PinkTrace::Bitness::DEFAULT]) => addr or nil
 *
 * Decodes the socket address at the given index.
 * If the system call's address argument was NULL, this function returns +nil+.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_socket_address(int argc, VALUE *argv, pink_unused VALUE mod)
{
	int save_errno;
	pid_t pid;
	unsigned bit, ind;
	pink_socket_address_t *addr, *caddr;
	VALUE addrKlass, addrObj;

	if (argc < 2 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		if (ind >= PINK_MAX_INDEX)
			rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
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
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	addr = ALLOC(pink_socket_address_t);

	if (!pink_decode_socket_address(pid, bit, ind, NULL, addr)) {
		save_errno = errno;
		free(addr);
		errno = save_errno;
		rb_sys_fail("pink_decode_socket_address()");
	}

	switch (addr->family) {
	case -1:
		/* Special case, the argument was NULL */
		free(addr);
		return Qnil;
	case AF_UNIX:
		addrKlass = pinkrb_cUNIXAddress;
		break;
	case AF_INET:
		addrKlass = pinkrb_cINETAddress;
		break;
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
		addrKlass = pinkrb_cINET6Address;
		break;
#endif
	default:
		rb_raise(pinkrb_eAddressError, "Unsupported address family: %d", addr->family);
	}

	addrObj = Data_Make_Struct(addrKlass, pink_socket_address_t, NULL, free, caddr);
	caddr->family = addr->family;
	memcpy(&caddr->u, &addr->u, sizeof(addr->u));
	free(addr);

	return addrObj;
}

/*
 * Document-method: PinkTrace::Socket.decode_address2
 * call-seq: PinkTrace::Socket.decode_address_fd(pid, index, [bitness=PinkTrace::Bitness::DEFAULT]) => addr|nil, fd
 *
 * Decodes the socket address at the given index and the file descriptor at index 0.
 * If the system call's address argument was NULL this function returns +nil+
 * and the file descriptor.
 *
 * Note: PinkTrace::IndexError is raised if +index+ argument is not smaller
 * than PinkTrace::MAX_INDEX.
 *
 * Note: PinkTrace::BitnessError is raised if +bitness+ is either unsupported
 * or undefined.
 */
static VALUE
pinkrb_decode_socket_address_fd(int argc, VALUE *argv, pink_unused VALUE mod)
{
	int save_errno;
	pid_t pid;
	unsigned bit, ind;
	long fd;
	pink_socket_address_t *addr, *caddr;
	VALUE addrKlass, addrObj;

	if (argc < 2 || argc > 3)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2INT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1])) {
		ind = FIX2UINT(argv[1]);
		if (ind >= PINK_MAX_INDEX)
			rb_raise(pinkrb_eIndexError, "index not smaller than MAX_INDEX");
	}
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc > 2) {
		if (FIXNUM_P(argv[2])) {
			bit = FIX2UINT(argv[2]);
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
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	addr = ALLOC(pink_socket_address_t);

	if (!pink_decode_socket_address(pid, bit, ind, &fd, addr)) {
		save_errno = errno;
		free(addr);
		errno = save_errno;
		rb_sys_fail("pink_decode_socket_address()");
	}

	switch (addr->family) {
	case -1:
		/* Special case, the argument was NULL */
		free(addr);
		return rb_assoc_new(Qnil, LONG2NUM(fd));
	case AF_UNIX:
		addrKlass = pinkrb_cUNIXAddress;
		break;
	case AF_INET:
		addrKlass = pinkrb_cINETAddress;
		break;
#if PINKTRACE_HAVE_IPV6
	case AF_INET6:
		addrKlass = pinkrb_cINET6Address;
		break;
#endif
	default:
		rb_raise(pinkrb_eAddressError, "Unsupported address family: %d", addr->family);
	}

	addrObj = Data_Make_Struct(addrKlass, pink_socket_address_t, NULL, free, caddr);
	caddr->family = addr->family;
	memcpy(&caddr->u, &addr->u, sizeof(addr->u));
	free(addr);

	return rb_assoc_new(addrObj, LONG2NUM(fd));
}

/*
 * Document-method: path
 * call-seq: addr.path => String
 *
 * Returns the path of the unix socket.
 */
static VALUE
pinkrb_unix_path(VALUE self)
{
	size_t len;
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family != AF_UNIX)
		rb_bug("Unsupported family, expected: AF_UNIX, got: %d", addr->family);

	if (addr->u.sa_un.sun_path[0] == '\0' && addr->u.sa_un.sun_path[1] != '\0')
		/* Abstract UNIX socket */
		len = strlen(addr->u.sa_un.sun_path + 1) + 1;
	else
		len = strlen(addr->u.sa_un.sun_path);

	return rb_str_new(addr->u.sa_un.sun_path, len);
}

/*
 * Document-method: abstract?
 * call-seq: addr.abstract? => true or false
 *
 * Returns +true+ if the UNIX socket is abstract.
 */
static VALUE
pinkrb_unix_abstract(VALUE self)
{
	pink_socket_address_t *addr;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family != AF_UNIX)
		rb_bug("Unsupported family, expected: AF_UNIX, got: %d", addr->family);

	if (addr->u.sa_un.sun_path[0] == '\0' && addr->u.sa_un.sun_path[1] != '\0')
		return Qtrue;
	return Qfalse;
}

/*
 * Document-method: ntop
 * call-seq: addr.ntop([max=PinkTrace::Socket::INET_ADDRSTRLEN]) => String
 *
 * Returns a string representation of the INET address.
 */
static VALUE
pinkrb_inet_ntop(int argc, VALUE *argv, VALUE self)
{
	int save_errno;
	unsigned max;
	char *ip;
	pink_socket_address_t *addr;
	VALUE ipv;

	if (argc > 1)
		rb_raise(rb_eArgError, "Wrong number of arguments");
	else if (argc > 0) {
		if (FIXNUM_P(argv[0]))
			max = FIX2UINT(argv[0]);
		else
			rb_raise(rb_eTypeError, "First argument is not a Fixnum");
	}
	else
		max = INET_ADDRSTRLEN;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family != AF_INET)
		rb_bug("Unsupported family, expected: AF_INET, got: %d", addr->family);

	ip = ALLOC_N(char, max);

	if (!inet_ntop(AF_INET, &addr->u.sa_in.sin_addr, ip, max)) {
		save_errno = errno;
		free(ip);
		errno = save_errno;
		rb_sys_fail("inet_ntop(3)");
	}

	ipv = rb_str_new2(ip);
	free(ip);
	return ipv;
}

/*
 * Document-method: ntop6
 * call-seq: addr.ntop6([max=PinkTrace::Socket::INET6_ADDRSTRLEN]) => String
 *
 * Returns a string representation of the INET6 address.
 */
#if PINKTRACE_HAVE_IPV6
static VALUE
pinkrb_inet6_ntop(int argc, VALUE *argv, VALUE self)
{
	int save_errno;
	unsigned max;
	char *ip;
	pink_socket_address_t *addr;
	VALUE ipv;

	if (argc > 1)
		rb_raise(rb_eArgError, "Wrong number of arguments");
	else if (argc > 0) {
		if (FIXNUM_P(argv[0]))
			max = FIX2UINT(argv[0]);
		else
			rb_raise(rb_eTypeError, "First argument is not a Fixnum");
	}
	else
		max = INET6_ADDRSTRLEN;

	Data_Get_Struct(self, pink_socket_address_t, addr);
	if (addr->family != AF_INET6)
		rb_bug("Unsupported family, expected: AF_INET6, got: %d", addr->family);

	ip = ALLOC_N(char, max);

	if (!inet_ntop(AF_INET6, &addr->u.sa6.sin6_addr, ip, max)) {
		save_errno = errno;
		free(ip);
		errno = save_errno;
		rb_sys_fail("inet_ntop(3)");
	}

	ipv = rb_str_new2(ip);
	free(ip);
	return ipv;
}
#endif

void
Init_PinkTrace(void)
{
	VALUE mod;
	VALUE trace_mod;
	VALUE event_mod;
	VALUE bitness_mod;
	VALUE syscall_mod;
	VALUE string_mod;
	VALUE socket_mod;


	/* PinkTrace module */
	mod = rb_define_module("PinkTrace");
	pinkrb_eAddressError = rb_define_class_under(mod, "AddressError", rb_eStandardError);
	pinkrb_eBitnessError = rb_define_class_under(mod, "BitnessError", rb_eStandardError);
	pinkrb_eEventError = rb_define_class_under(mod, "EventError", rb_eStandardError);
	pinkrb_eIndexError = rb_define_class_under(mod, "IndexError", rb_eIndexError);

	/* Global Constants */
#if PINKTRACE_HAVE_IPV6
	rb_define_const(mod, "HAVE_IPV6", Qtrue);
#else
	rb_define_const(mod, "HAVE_IPV6", Qfalse);
#endif /* PINKTRACE_HAVE_IPV6 */
	/* about.h */
	rb_define_const(mod, "VERSION", INT2FIX(PINKTRACE_VERSION));
	rb_define_const(mod, "VERSION_MAJOR", INT2FIX(PINKTRACE_VERSION_MAJOR));
	rb_define_const(mod, "VERSION_MINOR", INT2FIX(PINKTRACE_VERSION_MINOR));
	rb_define_const(mod, "VERSION_MICRO", INT2FIX(PINKTRACE_VERSION_MICRO));
	rb_define_const(mod, "VERSION_SUFFIX", rb_str_new2(PINKTRACE_VERSION_SUFFIX));
	rb_define_const(mod, "GIT_HEAD", rb_str_new2(PINKTRACE_GIT_HEAD));
	/* util.h */
	rb_define_const(mod, "MAX_INDEX", INT2FIX(PINK_MAX_INDEX));

	/* trace.h */
	trace_mod = rb_define_module_under(mod, "Trace");
	rb_define_const(trace_mod, "SYSGOOD", INT2FIX(PINK_TRACE_OPTION_SYSGOOD));
	rb_define_const(trace_mod, "FORK", INT2FIX(PINK_TRACE_OPTION_FORK));
	rb_define_const(trace_mod, "VFORK", INT2FIX(PINK_TRACE_OPTION_VFORK));
	rb_define_const(trace_mod, "CLONE", INT2FIX(PINK_TRACE_OPTION_CLONE));
	rb_define_const(trace_mod, "EXEC", INT2FIX(PINK_TRACE_OPTION_EXEC));
	rb_define_const(trace_mod, "VFORK_DONE", INT2FIX(PINK_TRACE_OPTION_VFORK_DONE));
	rb_define_const(trace_mod, "EXIT", INT2FIX(PINK_TRACE_OPTION_EXIT));
	rb_define_const(trace_mod, "ALL", INT2FIX(PINK_TRACE_OPTION_ALL));
	rb_define_module_function(trace_mod, "me", pinkrb_trace_me, 0);
	rb_define_module_function(trace_mod, "cont", pinkrb_trace_cont, -1);
	rb_define_module_function(trace_mod, "kill", pinkrb_trace_kill, 1);
	rb_define_module_function(trace_mod, "singlestep", pinkrb_trace_singlestep, -1);
	rb_define_module_function(trace_mod, "syscall", pinkrb_trace_syscall, -1);
	rb_define_module_function(trace_mod, "geteventmsg", pinkrb_trace_geteventmsg, 1);
	rb_define_module_function(trace_mod, "setup", pinkrb_trace_setup, -1);
	rb_define_module_function(trace_mod, "attach", pinkrb_trace_attach, 1);
	rb_define_module_function(trace_mod, "detach", pinkrb_trace_detach, -1);

	/* fork.h */
	rb_define_module_function(mod, "fork", pinkrb_fork, -1);

	/* event.h */
	event_mod = rb_define_module_under(mod, "Event");
	rb_define_const(event_mod, "STOP", INT2FIX(PINK_EVENT_STOP));
	rb_define_const(event_mod, "SYSCALL", INT2FIX(PINK_EVENT_SYSCALL));
	rb_define_const(event_mod, "FORK", INT2FIX(PINK_EVENT_FORK));
	rb_define_const(event_mod, "VFORK", INT2FIX(PINK_EVENT_VFORK));
	rb_define_const(event_mod, "CLONE", INT2FIX(PINK_EVENT_CLONE));
	rb_define_const(event_mod, "VFORK_DONE", INT2FIX(PINK_EVENT_VFORK_DONE));
	rb_define_const(event_mod, "EXEC", INT2FIX(PINK_EVENT_EXEC));
	rb_define_const(event_mod, "EXIT", INT2FIX(PINK_EVENT_EXIT));
	rb_define_const(event_mod, "GENUINE", INT2FIX(PINK_EVENT_GENUINE));
	rb_define_const(event_mod, "EXIT_GENUINE", INT2FIX(PINK_EVENT_EXIT_GENUINE));
	rb_define_const(event_mod, "EXIT_SIGNAL", INT2FIX(PINK_EVENT_EXIT_SIGNAL));
	rb_define_module_function(event_mod, "decide", pinkrb_event_decide, -1);

	/* bitness.h */
	bitness_mod = rb_define_module_under(mod, "Bitness");
	rb_define_const(bitness_mod, "SUPPORTED", INT2FIX(PINKTRACE_SUPPORTED_BITNESS));
	rb_define_const(bitness_mod, "DEFAULT", INT2FIX(PINKTRACE_DEFAULT_BITNESS));
	rb_define_const(bitness_mod, "BITNESS_32", INT2FIX(PINK_BITNESS_32));
	rb_define_const(bitness_mod, "BITNESS_64", INT2FIX(PINK_BITNESS_64));
	rb_define_module_function(bitness_mod, "get", pinkrb_bitness_get, 1);

	/* util.h && name.h */
	syscall_mod = rb_define_module_under(mod, "SysCall");
	rb_define_module_function(syscall_mod, "name", pinkrb_name_syscall, -1);
	rb_define_module_function(syscall_mod, "get_no", pinkrb_util_get_syscall, 1);
	rb_define_module_function(syscall_mod, "set_no", pinkrb_util_set_syscall, 2);
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
	rb_define_const(socket_mod, "INET_ADDRSTRLEN", INT2FIX(INET_ADDRSTRLEN));
#ifdef PINKTRACE_HAVE_IPV6
	rb_define_const(socket_mod, "INET6_ADDRSTRLEN", INT2FIX(INET6_ADDRSTRLEN));
#endif /* PINKTRACE_HAVE_IPV6 */
	rb_define_module_function(socket_mod, "name", pinkrb_name_socket_subcall, 1);
	rb_define_module_function(socket_mod, "decode_call", pinkrb_decode_socket_call, -1);
	rb_define_module_function(socket_mod, "decode_fd", pinkrb_decode_socket_fd, -1);

	/* Address Objects */
	pinkrb_cAddress = rb_define_class_under(socket_mod, "Address", rb_cObject);
	pinkrb_cUNIXAddress = rb_define_class_under(socket_mod, "UNIXAddress", pinkrb_cAddress);
	pinkrb_cINETAddress = rb_define_class_under(socket_mod, "INETAddress", pinkrb_cAddress);
#if PINKTRACE_HAVE_IPV6
	pinkrb_cINET6Address = rb_define_class_under(socket_mod, "INET6Address", pinkrb_cAddress);
#endif /* PINKTRACE_HAVE_IPV6 */

	/* The address objects are only returned by PinkTrace::Socket.decode_address;
	 * thus we don't need an initialize method. */
	rb_undef_method(pinkrb_cAddress, "initialize");

	/* UNIX Address methods */
	rb_define_method(pinkrb_cUNIXAddress, "path", pinkrb_unix_path, 0);
	rb_define_alias(pinkrb_cUNIXAddress, "to_s", "path");
	rb_define_method(pinkrb_cUNIXAddress, "abstract?", pinkrb_unix_abstract, 0);

	/* INET Address methods */
	rb_define_method(pinkrb_cINETAddress, "ntop", pinkrb_inet_ntop, -1);
	rb_define_alias(pinkrb_cINETAddress, "to_s", "ntop");

#if PINKTRACE_HAVE_IPV6
	/* INET6 Address methods */
	rb_define_method(pinkrb_cINET6Address, "ntop6", pinkrb_inet6_ntop, -1);
	rb_define_alias(pinkrb_cINET6Address, "to_s", "ntop6");
#endif /* PINKTRACE_HAVE_IPV6 */

	rb_define_module_function(socket_mod, "decode_address", pinkrb_decode_socket_address, -1);
	rb_define_module_function(socket_mod, "decode_address_fd", pinkrb_decode_socket_address_fd, -1);
}
