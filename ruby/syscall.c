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
 * Document-method: PinkTrace::Syscall.name
 * call-seq:
 *   PinkTrace::Syscall.name(scno, [bitness=PinkTrace::Bitness::DEFAULT]) -> String or nil
 *
 * Return the name of the given system call.
 */
VALUE
pinkrb_name_syscall(int argc, VALUE *argv, VALUE mod)
{
	unsigned bit;
	long scno;
	const char *scname;
	VALUE vscno, vbit;

	switch (rb_scan_args(argc, argv, "11", &vscno, &vbit)) {
	case 1:
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 2:
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	scno = NUM2LONG(vscno);

	scname = pink_name_syscall(scno, bit);
	return scname ? rb_str_new2(scname) : Qnil;
}

/*
 * Document-method: PinkTrace::Syscall.lookup
 * call-seq:
 *   PinkTrace::Syscall.lookup(name, [bitness=PinkTrace::Bitness::DEFAULT]) -> fixnum
 *
 * Look up the given system call name.
 * Returns -1 if the lookup fails.
 */
VALUE
pinkrb_name_lookup(int argc, VALUE *argv, VALUE mod)
{
	unsigned bit;
	const char *name;
	VALUE vname, vbit;

#if !defined(RSTRING_PTR)
#define RSTRING_PTR(v) (RSTRING((v))->ptr)
#endif /* !defined(RSTRING_PTR) */

	switch (rb_scan_args(argc, argv, "11", &vname, &vbit)) {
	case 1:
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 2:
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	SafeStringValue(vname);
	name = RSTRING_PTR(vname);

	return LONG2NUM(pink_name_lookup(name, bit));
}

/*
 * Document-method: PinkTrace::Syscall.get_no
 * call-seq:
 *   PinkTrace::Syscall.get_no(pid, [bitness=PinkTrace::Bitness::DEFAULT]) -> fixnum
 *
 * Returns the last system call number called by the traced child.
 */
VALUE
pinkrb_util_get_syscall(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit;
	long scno;
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

	if (!pink_util_get_syscall(pid, bit, &scno))
		rb_sys_fail("pink_util_get_syscall()");

	return LONG2NUM(scno);
}

/*
 * Document-method: PinkTrace::Syscall.set_no
 * call-seq:
 *   PinkTrace::Syscall.set_no(pid, scno, [bitness=PinkTrace::Bitness::DEFAULT]) -> nil
 *
 * Sets the system call number for the traced child.
 */
VALUE
pinkrb_util_set_syscall(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit;
	long scno;
	VALUE vpid, vscno, vbit;

	switch (rb_scan_args(argc, argv, "21", &vpid, &vscno, &vbit)) {
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
	scno = NUM2LONG(vscno);

	if (!pink_util_set_syscall(pid, bit, scno))
		rb_sys_fail("pink_util_set_syscall()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Syscall.get_ret
 * call-seq:
 *   PinkTrace::Syscall.get_ret(pid) -> fixnum
 *
 * Returns the return value of the last system call called by the traced child.
 */
VALUE
pinkrb_util_get_return(VALUE mod, VALUE vpid)
{
	pid_t pid;
	long ret;

	pid = NUM2PIDT(vpid);
	if (!pink_util_get_return(pid, &ret))
		rb_sys_fail("pink_util_get_return()");

	return LONG2FIX(ret);
}

/*
 * Document-method: PinkTrace::Syscall.set_ret
 * call-seq:
 *   PinkTrace::Syscall.set_ret(pid, ret) -> nil
 *
 * Set the return value of the system call for the traced child.
 */
VALUE
pinkrb_util_set_return(VALUE mod, VALUE vpid, VALUE vret)
{
	pid_t pid;
	long ret;

	pid = NUM2PIDT(vpid);
	ret = NUM2LONG(vret);

	if (!pink_util_set_return(pid, ret))
		rb_sys_fail("pink_util_set_return()");

	return Qnil;
}

/*
 * Document-method: PinkTrace::Syscall.get_arg
 * call-seq:
 *   PinkTrace::Syscall.get_arg(pid, index, [bitness=PinkTrace::Bitness::Default]) -> fixnum
 *
 * Returns the system call argument at the given index for the traced child.
 *
 */
VALUE
pinkrb_util_get_arg(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	long arg;
	VALUE vpid, vind, vbit;

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

	if (!pink_util_get_arg(pid, bit, ind, &arg))
		rb_sys_fail("pink_util_get_arg()");

	return LONG2NUM(arg);
}

/*
 * Document-method: PinkTrace::Syscall.set_arg
 * call-seq:
 *   PinkTrace::Syscall.set_arg(pid, index, arg, [bitness=PinkTrace::Bitness::Default]) -> nil
 *
 * Sets the system call argument at the specified index to the given value.
 */
VALUE
pinkrb_util_set_arg(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	long arg;
	VALUE vpid, vind, varg, vbit;

	switch (rb_scan_args(argc, argv, "31", &vpid, &vind, &varg, &vbit)) {
	case 3:
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 4:
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);
	ind = FIX2UINT(vind);
	arg = NUM2LONG(varg);

	if (!pink_util_set_arg(pid, bit, ind, arg))
		rb_sys_fail("pink_util_set_arg()");

	return Qnil;
}
