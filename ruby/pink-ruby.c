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
#include <ruby.h>

/* Prototypes */
void
Init_PinkTrace(void);

/* Global variables */
static VALUE pinkrb_eUnknownEventError;

/*
 * Document-class: PinkTrace
 *
 * == Summary
 *
 * Ruby extension to the pinktrace library.
 *
 * == Classes
 *
 * Following are the classes that are most likely to be of interest to the user:
 *   - PinkTrace::Trace
 *   - PinkTrace::Event
 *   - PinkTrace::Bitness
 *   - PinkTrace::Util
 *
 * == Constants
 *
 *   PinkTrace::VERSION_MAJOR
 *     The major version (eg 0.4.1 -> 0)
 *   PinkTrace::VERSION_MINOR
 *     The minor version (eg 0.4.1 -> 4)
 *   PinkTrace::VERSION_MICRO
 *     The micro version (eg 0.4.1 -> 1)
 *   PinkTrace::VERSION
 *     The version, two digits per part (eg 1.3.5 -> 10305)
 *   PinkTrace::VERSION_SUFFIX
 *     The version suffix (eg "_alpha1"), often an empty string
 *   PinkTrace::GIT_HEAD
 *     The Git head used to build this binary, if applicable (eg "deadbeef" or "1.0.0-40-f00-dirty" or "")
 */

/*
 * Document-class: PinkTrace::Trace
 *
 * This class includes thin wrappers around <tt>ptrace()</tt> system call.
 *
 * == Constants
 *
 *   PinkTrace::Trace::SYSGOOD
 *     This constant represents the trace option SYSGOOD.
 *     If this flag is set in the options argument of PinkTrace::Trace.setup,
 *     when delivering syscall traps, bit 7 is set in signal number (i.e.,
 *     deliver (SIGTRAP | 0x80) This makes it easy for the tracer to tell the
 *     difference between normal traps and those caused by a sycall. This
 *     option may not work on all architectures.
 *
 *   TODO: Document other constants
 */

/*
 * Document-method: PinkTrace::Trace.me
 * call-seq: PinkTrace::Trace.me
 *
 * Indicates that this process is to be traced by its parent.
 */
static VALUE
pinkrb_trace_me(pink_unused VALUE mod)
{
	if (!pink_trace_me())
		rb_sys_fail("ptrace(PTRACE_TRACEME, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.cont
 * call-seq: PinkTrace::Trace.cont(pid, sig=0) => nil
 *
 * Restarts the stopped child process.
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
		rb_sys_fail("ptrace(PTRACE_CONT, ...)");

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
		rb_sys_fail("ptrace(PTRACE_KILL, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.singlestep
 * call-seq: PinkTrace::Trace.singlestep(pid, sig=0) => nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * execution of a single instruction.
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
		rb_sys_fail("ptrace(PTRACE_SINGLESTEP, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.syscall
 * call-seq: PinkTrace::Trace.syscall(pid, sig=0) => nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * the entry or exit of the next system call.
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
		rb_sys_fail("ptrace(PTRACE_SYSCALL, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.geteventmsg
 * call-seq: PinkTrace::Trace.geteventmsg(pid) => fixnum
 *
 * Returns a message (as a Fixnum) about the trace event that just
 * happened, For EXIT event this is the child's exit status. For FORK, VFORK,
 * CLONE and VFORK_DONE events this is the process ID of the new process.
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
		rb_sys_fail("ptrace(PTRACE_GETEVENTMSG, ...)");

	return ULONG2NUM(data);
}

/*
 * Document-method: PinkTrace::Trace.setup
 * call-seq: PinkTrace::Trace.setup(pid, options=PinkTrace::Trace::SYSGOOD) => nil
 *
 * Sets the tracing options
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
		rb_sys_fail("ptrace(PTRACE_SETOPTIONS, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.attach
 * call-seq: PinkTrace::Trace.attach(pid) => nil
 *
 * Attaches to the process specified in pid, making it a traced "child" of the
 * calling process.
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
		rb_sys_fail("ptrace(PTRACE_ATTACH, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.detach
 * call-seq: PinkTrace::Trace.detach(pid, sig=0) => nil
 *
 * Restarts the stopped child as for PinkTrace::Trace.cont, but first detaches
 * from the process, undoing the reparenting effect of PinkTrace::Trace.attach.
 */
static VALUE
pinkrb_trace_detach(pink_unused VALUE mod, VALUE pidv, VALUE sigv)
{
	pid_t pid;
	long sig;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "Process ID is not a Fixnum");

	if (FIXNUM_P(sigv))
		sig = FIX2LONG(sigv);
	else
		rb_raise(rb_eTypeError, "Signal is not a Fixnum");

	if (!pink_trace_detach(pid, sig))
		rb_sys_fail("ptrace(PTRACE_DETACH, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace.fork
 * call-seq: PinkTrace.fork(opts=PinkTrace::Trace::SYSGOOD) [{ block }] => fixnum or nil
 *
 * fork(2) wrapper that sets up the child for tracing.
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
			rb_sys_fail("ptrace(PTRACE_TRACEME, ...");
		case PINK_ERROR_TRACE_SETUP:
			rb_sys_fail("ptrace(PTRACE_SETOPTIONS, ...");
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
 *   PinkTrace::Event::STOP
 *     The traced child has received a SIGSTOP.
 *   PinkTrace::Event::SYSCALL
 *     The traced child is entering or exiting a system call.
 *   PinkTrace::Event::FORK
 *     The traced child called fork(2).
 *   PinkTrace::Event::VFORK
 *     The traced child called vfork(2).
 *   PinkTrace::Event::CLONE
 *     The traced child called clone(2).
 *   PinkTrace::Event::VFORK_DONE
 *     The traced child is exiting a vfork(2) call.
 *   PinkTrace::Event::EXEC
 *     The traced child is exiting. (ptrace way, stopped before exit)
 *   PinkTrace::Event::GENUINE
 *     The traced child has received a genuine signal.
 *   PinkTrace::Event::EXIT_GENUINE
 *     The traced child has exited normally.
 *   PinkTrace::Event::EXIT_SIGNAL
 *     The traced child has been terminated with a signal.
 *
 * == Exceptions
 *   PinkTrace::Event::UnknownEventError
 *     Raised by PinkTrace::Event.decide in case the event is unknown
 */

/*
 * Document-class: PinkTrace::Event::UnknownEventError
 *
 * Raised by PinkTrace::Event.decide in case the event is unknown
 */

/*
 * Document-method: PinkTrace::Event.decide
 * call-seq: PinkTrace::Event.decide(status=$?.status) => fixnum
 *
 * Returns the last event made by child.
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
	else
		status = FIX2INT(rb_iv_get(rb_last_status_get(), "status"));

	event = pink_event_decide(status);
	if (event == PINK_EVENT_UNKNOWN)
		rb_raise(pinkrb_eUnknownEventError, "Unknown event (status: %#x)", status);

	return UINT2NUM(event);
}

/*
 * Document-class: PinkTrace::Bitness
 *
 * This class defines constants and functions about bitness.
 *
 * == Constants
 *   PinkTrace::Bitness::SUPPORTED
 *     Number of supported bitnesses (eg 2 on x86_64, 1 on i386)
 *   PinkTrace::Bitness::DEFAULT
 *     The default bitness
 *   PinkTrace::Bitness::BITNESS_32
 *     32 bit mode
 *   PinkTrace::Bitness::BITNESS_64
 *     64 bit mode
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
	int bitness;

	if (FIXNUM_P(pidv))
		pid = FIX2INT(pidv);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	bitness = pink_bitness_get(pid);
	if (bitness == PINK_BITNESS_UNKNOWN)
		rb_sys_fail("ptrace(PTRACE_PEEKUSER, ...)");

	return INT2FIX(bitness);
}

/*
 * Document-class: PinkTrace::SysCall
 *
 * This class defines utilities useful when tracing processes.
 */

/*
 * Document-method: PinkTrace::SysCall.name
 * call-seq: PinkTrace::SysCall.name(scno, bitness=PinkTrace::Bitness::DEFAULT) => String or nil
 *
 * Return the name of the given system call.
 */
static VALUE
pinkrb_name_syscall(int argc, VALUE *argv, pink_unused VALUE mod)
{
	unsigned bitness;
	long scno;
	const char *scname;

	if (argc < 1 || argc > 2)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		scno = FIX2LONG(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (argc == 2) {
		if (FIXNUM_P(argv[1]))
			bitness = FIX2UINT(argv[1]);
		else
			rb_raise(rb_eTypeError, "Second argument is not a Fixnum");
	}
	else
		bitness = PINKTRACE_DEFAULT_BITNESS;

	scname = pink_name_syscall(scno, bitness);
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
		rb_sys_fail("ptrace(PTRACE_PEEKUSER, ...)");

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
		rb_sys_fail("ptrace(PTRACE_POKEUSER, ...)");

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
		rb_sys_fail("ptrace(PTRACE_PEEKUSER)");

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
		rb_sys_fail("ptrace(PTRACE_POKEUSER, ...)");

	return Qnil;
}

/*
 * Document-method: PinkTrace::SysCall.get_arg
 * call-seq: PinkTrace::SysCall.get_arg(pid, index, [bitness=PinkTrace::Bitness::Default]) => fixnum
 *
 * Returns the system call argument at the given index for the traced child.
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

	if (FIXNUM_P(argv[1]))
		ind = FIX2UINT(argv[1]);
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc == 3) {
		if (FIXNUM_P(argv[2]))
			bit = FIX2UINT(argv[2]);
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	if (!pink_util_get_arg(pid, bit, ind, &arg))
		rb_sys_fail("ptrace(PTRACE_PEEKUSER, ...)");

	return LONG2FIX(arg);
}

/*
 * Document-class: PinkTrace::String
 *
 * This class contains functions to decode/encode string arguments.
 */

/*
 * Document-method: PinkTrace::String.decode
 * call-seq: PinkTrace::String.decode(pid, index, [[max=0], [bitness=PinkTrace::Bitness::DEFAULT]]) => String
 *
 * This function decodes the string at the argument of the given index.
 */
static VALUE
pinkrb_decode_string(int argc, VALUE *argv, pink_unused VALUE mod)
{
	pid_t pid;
	unsigned bit, ind, max;
	char *str;
	VALUE ret;

	if (argc < 2 || argc > 4)
		rb_raise(rb_eArgError, "Wrong number of arguments");

	if (FIXNUM_P(argv[0]))
		pid = FIX2UINT(argv[0]);
	else
		rb_raise(rb_eTypeError, "First argument is not a Fixnum");

	if (FIXNUM_P(argv[1]))
		ind = FIX2UINT(argv[1]);
	else
		rb_raise(rb_eTypeError, "Second argument is not a Fixnum");

	if (argc >= 3) {
		if (FIXNUM_P(argv[2]))
			max = FIX2UINT(argv[2]);
		else
			rb_raise(rb_eTypeError, "Third argument is not a Fixnum");
	}
	else
		max = 0;

	if (argc > 3) {
		if (FIXNUM_P(argv[3]))
			bit = FIX2UINT(argv[3]);
		else
			rb_raise(rb_eTypeError, "Fourth argument is not a Fixnum");
	}
	else
		bit = PINKTRACE_DEFAULT_BITNESS;

	if (!max) {
		/* Use pink_decode_string_persistent() */
		str = pink_decode_string_persistent(pid, bit, ind);
		if (!str)
			rb_sys_fail("pink_decode_string_persistent()");

		ret = rb_str_new2(str);
		free(str);
		return ret;
	}

	/* Use pink_decode_string() */
	str = ALLOC_N(char, max);
	if (!pink_decode_string(pid, bit, ind, str, max))
		rb_sys_fail("pink_decode_string()");

	ret = rb_str_new2(str);
	if (str)
		free(str);
	return ret;
}

void
Init_PinkTrace(void)
{
	VALUE mod, tmod, emod, bmod, smod, stmod;

	/* PinkTrace module */
	mod = rb_define_module("PinkTrace");

	/* about.h */
	rb_define_const(mod, "VERSION", INT2FIX(PINKTRACE_VERSION));
	rb_define_const(mod, "VERSION_MAJOR", INT2FIX(PINKTRACE_VERSION_MAJOR));
	rb_define_const(mod, "VERSION_MINOR", INT2FIX(PINKTRACE_VERSION_MINOR));
	rb_define_const(mod, "VERSION_MICRO", INT2FIX(PINKTRACE_VERSION_MICRO));
	rb_define_const(mod, "VERSION_SUFFIX", rb_str_new2(PINKTRACE_VERSION_SUFFIX));
	rb_define_const(mod, "GIT_HEAD", rb_str_new2(PINKTRACE_GIT_HEAD));

	/* trace.h */
	tmod = rb_define_module_under(mod, "Trace");
	rb_define_const(tmod, "SYSGOOD", INT2FIX(PINK_TRACE_OPTION_SYSGOOD));
	rb_define_const(tmod, "FORK", INT2FIX(PINK_TRACE_OPTION_FORK));
	rb_define_const(tmod, "VFORK", INT2FIX(PINK_TRACE_OPTION_VFORK));
	rb_define_const(tmod, "CLONE", INT2FIX(PINK_TRACE_OPTION_CLONE));
	rb_define_const(tmod, "EXEC", INT2FIX(PINK_TRACE_OPTION_EXEC));
	rb_define_const(tmod, "VFORK_DONE", INT2FIX(PINK_TRACE_OPTION_VFORK_DONE));
	rb_define_const(tmod, "EXIT", INT2FIX(PINK_TRACE_OPTION_EXIT));
	rb_define_const(tmod, "ALL", INT2FIX(PINK_TRACE_OPTION_ALL));
	rb_define_module_function(tmod, "me", pinkrb_trace_me, 0);
	rb_define_module_function(tmod, "cont", pinkrb_trace_cont, -1);
	rb_define_module_function(tmod, "kill", pinkrb_trace_kill, 1);
	rb_define_module_function(tmod, "singlestep", pinkrb_trace_singlestep, -1);
	rb_define_module_function(tmod, "syscall", pinkrb_trace_syscall, -1);
	rb_define_module_function(tmod, "geteventmsg", pinkrb_trace_geteventmsg, 1);
	rb_define_module_function(tmod, "setup", pinkrb_trace_setup, -1);
	rb_define_module_function(tmod, "attach", pinkrb_trace_attach, 2);
	rb_define_module_function(tmod, "detach", pinkrb_trace_detach, 2);

	/* fork.h */
	rb_define_module_function(mod, "fork", pinkrb_fork, -1);

	/* event.h */
	emod = rb_define_module_under(mod, "Event");
	pinkrb_eUnknownEventError = rb_define_class_under(emod, "UnknownEventError", rb_eStandardError);
	rb_define_const(emod, "STOP", INT2FIX(PINK_EVENT_STOP));
	rb_define_const(emod, "SYSCALL", INT2FIX(PINK_EVENT_SYSCALL));
	rb_define_const(emod, "FORK", INT2FIX(PINK_EVENT_FORK));
	rb_define_const(emod, "VFORK", INT2FIX(PINK_EVENT_VFORK));
	rb_define_const(emod, "CLONE", INT2FIX(PINK_EVENT_CLONE));
	rb_define_const(emod, "VFORK_DONE", INT2FIX(PINK_EVENT_VFORK_DONE));
	rb_define_const(emod, "EXEC", INT2FIX(PINK_EVENT_EXEC));
	rb_define_const(emod, "EXIT", INT2FIX(PINK_EVENT_EXIT));
	rb_define_const(emod, "GENUINE", INT2FIX(PINK_EVENT_GENUINE));
	rb_define_const(emod, "EXIT_GENUINE", INT2FIX(PINK_EVENT_EXIT_GENUINE));
	rb_define_const(emod, "EXIT_SIGNAL", INT2FIX(PINK_EVENT_EXIT_SIGNAL));
	rb_define_module_function(emod, "decide", pinkrb_event_decide, -1);

	/* bitness.h */
	bmod = rb_define_module_under(mod, "Bitness");
	rb_define_const(bmod, "SUPPORTED", INT2FIX(PINKTRACE_SUPPORTED_BITNESS));
	rb_define_const(bmod, "DEFAULT", INT2FIX(PINKTRACE_DEFAULT_BITNESS));
	rb_define_const(bmod, "BITNESS_32", INT2FIX(PINK_BITNESS_32));
	rb_define_const(bmod, "BITNESS_64", INT2FIX(PINK_BITNESS_64));
	rb_define_module_function(bmod, "get", pinkrb_bitness_get, 1);

	/* util.h && name.h */
	smod = rb_define_module_under(mod, "SysCall");
	rb_define_module_function(smod, "name", pinkrb_name_syscall, -1);
	rb_define_module_function(smod, "get_no", pinkrb_util_get_syscall, 1);
	rb_define_module_function(smod, "set_no", pinkrb_util_set_syscall, 2);
	rb_define_module_function(smod, "get_ret", pinkrb_util_get_return, 1);
	rb_define_module_function(smod, "set_ret", pinkrb_util_set_return, 2);
	rb_define_module_function(smod, "get_arg", pinkrb_util_get_arg, -1);

	/* decode.h && encode.h (only string {en,de}coding) */
	stmod = rb_define_module_under(mod, "String");
	rb_define_module_function(stmod, "decode", pinkrb_decode_string, -1);
	/* TODO: rb_define_module_function(stmod, "encode", pinkrb_encode_string, -1); */
}