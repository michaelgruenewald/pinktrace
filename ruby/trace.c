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
 * Document-method: PinkTrace::Trace.me
 * call-seq:
 *   PinkTrace::Trace.me() -> nil
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
VALUE
pinkrb_trace_me(VALUE mod)
{
	if (!pink_trace_me())
		rb_sys_fail("pink_trace_me()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.cont
 * call-seq:
 *   PinkTrace::Trace.cont(pid, [[sig=0], [addr=1]]) -> nil
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
VALUE
pinkrb_trace_cont(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	long sig, addr;
	VALUE vpid, vsig, vaddr;

	switch (rb_scan_args(argc, argv, "12", &vpid, &vsig, &vaddr)) {
	case 1:
		addr = 1;
		sig = 0;
		break;
	case 2:
		addr = 1;
		sig = NUM2LONG(vsig);
		break;
	case 3:
		addr = NUM2LONG(vaddr);
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_cont(pid, sig, (char *)addr))
		rb_sys_fail("pink_trace_cont()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.resume
 * call-seq:
 *   PinkTrace::Trace.resume(pid, [sig=0]) -> nil
 *
 * Resumes the stopped child process. This is equivalent to
 * PinkTrace::Trace.cont(pid, sig, 1)
 *
 * If +sig+ argument is non-zero and not SIGSTOP, it is interpreted as the
 * signal to be delivered to the child; otherwise, no signal is delivered.
 * Thus, for example, the parent can control whether a signal sent to the child
 * is delivered or not.
 */
VALUE
pinkrb_trace_resume(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_resume(pid, sig))
		rb_sys_fail("pink_trace_resume()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.kill
 * call-seq:
 *   PinkTrace::Trace.kill(pid) -> nil
 *
 * Kills the traced child process with SIGKILL.
 */
VALUE
pinkrb_trace_kill(VALUE mod, VALUE vpid)
{
	pid_t pid;

	pid = NUM2PIDT(vpid);
	if (!pink_trace_kill(pid))
		rb_sys_fail("pink_trace_kill()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.singlestep
 * call-seq:
 *   PinkTrace::Trace.singlestep(pid, [sig=0]) -> nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * execution of a single instruction.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 */
VALUE
pinkrb_trace_singlestep(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_singlestep(pid, sig))
		rb_sys_fail("pink_trace_singlestep()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.syscall
 * call-seq:
 *   PinkTrace::Trace.syscall(pid, [sig=0]) -> nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * the entry or exit of the next system call.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 */
VALUE
pinkrb_trace_syscall(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_syscall(pid, sig))
		rb_sys_fail("pink_trace_syscall()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.syscall_entry
 * call-seq:
 *   PinkTrace::Trace.syscall_entry(pid, [sig=0]) -> nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * the entry of next system call.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 *
 * Availability: FreeBSD
 */
VALUE
pinkrb_trace_syscall_entry(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_FREEBSD
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_syscall_entry(pid, sig))
		rb_sys_fail("pink_trace_syscall_entry()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Trace.syscall_exit
 * call-seq:
 *   PinkTrace::Trace.syscall_exit(pid, [sig=0]) -> nil
 *
 * Restarts the stopped child process and arranges it to be stopped after
 * the exit of next system call.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 *
 * Availability: FreeBSD
 */
VALUE
pinkrb_trace_syscall_exit(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_FREEBSD
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_syscall_exit(pid, sig))
		rb_sys_fail("pink_trace_syscall_exit()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif /* defined(PINKTRACE_FREEBSD) */
}

/*
 * Document-method: PinkTrace::Trace.sysemu
 * call-seq:
 *   PinkTrace::Trace.sysemu(pid, [sig=0]) -> nil
 *
 * Restarts the stopped child process and arranges it to be stopped after the
 * entry of the next system call which will *not* be executed.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 *
 * Availability: Linux
 */
VALUE
pinkrb_trace_sysemu(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_LINUX
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_sysemu(pid, sig))
		rb_sys_fail("pink_trace_sysemu()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Trace.sysemu_singlestep
 * call-seq:
 *   PinkTrace::Trace.sysemu_singlestep(pid, [sig=0]) -> nil
 *
 * Restarts the stopped child process PinkTrace::Trace.sysemu but also
 * singlesteps if not a system call.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 *
 * Availability: Linux
 */
VALUE
pinkrb_trace_sysemu_singlestep(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_LINUX
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_sysemu_singlestep(pid, sig))
		rb_sys_fail("pink_trace_sysemu_singlestep()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Trace.geteventmsg
 * call-seq:
 *   PinkTrace::Trace.geteventmsg(pid) -> fixnum
 *
 * Returns a message (as a <tt>fixnum</tt>) about the trace event that just
 * happened, For *EXIT* event this is the child's exit status. For *FORK*,
 * *VFORK*, *CLONE* and *VFORK_DONE* events this is the process ID of the new
 * process.
 *
 * Availability: Linux
 */
VALUE
pinkrb_trace_geteventmsg(VALUE mod, VALUE vpid)
{
#ifdef PINKTRACE_LINUX
	pid_t pid;
	unsigned long data;

	pid = NUM2PIDT(vpid);
	if (!pink_trace_geteventmsg(pid, &data))
		rb_sys_fail("pink_trace_geteventmsg()");

	return ULONG2NUM(data);
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Trace.setup
 * call-seq:
 *   PinkTrace::Trace.setup(pid, [options=0]) -> nil
 *
 * Sets the tracing options.
 *
 * Availability: Linux
 */
VALUE
pinkrb_trace_setup(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_LINUX
	pid_t pid;
	int opts;
	VALUE vpid, vopts;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vopts)) {
	case 1:
		opts = 0;
		break;
	case 2:
		opts = NUM2INT(vopts);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_setup(pid, opts))
		rb_sys_fail("pink_trace_setup()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::Trace.attach
 * call-seq:
 *   PinkTrace::Trace.attach(pid) -> nil
 *
 * Attaches to the process specified in pid, making it a traced "child" of the
 * calling process; the behaviour of the child is as if it had done a
 * PinkTrace::Trace.me. The child is sent a SIGSTOP, but will not necessarily have
 * stopped by the completion of this call; use Process.waitpid to wait for the
 * child to stop.
 */
VALUE
pinkrb_trace_attach(VALUE mod, VALUE vpid)
{
	pid_t pid;

	pid = NUM2PIDT(vpid);
	if (!pink_trace_attach(pid))
		rb_sys_fail("pink_trace_attach()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Trace.detach
 * call-seq:
 *   PinkTrace::Trace.detach(pid, [sig=0]) -> nil
 *
 * Restarts the stopped child as for PinkTrace::Trace.cont, but first detaches
 * from the process, undoing the reparenting effect of PinkTrace::Trace.attach.
 *
 * The +sig+ argument is treated as the same way as the +sig+ argument of
 * PinkTrace::Trace.cont.
 */
VALUE
pinkrb_trace_detach(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	long sig;
	VALUE vpid, vsig;

	switch (rb_scan_args(argc, argv, "11", &vpid, &vsig)) {
	case 1:
		sig = 0;
		break;
	case 2:
		sig = NUM2LONG(vsig);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);

	if (!pink_trace_detach(pid, sig))
		rb_sys_fail("pink_trace_detach()");

	return Qnil;
}
