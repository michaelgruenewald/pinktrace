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
 * Document-method: PinkTrace::String.decode
 * call-seq:
 *   PinkTrace::String.decode(pid, index, [[maxlen=-1], [bitness=PinkTrace::Bitness::DEFAULT]]) -> String
 *
 * This function decodes the string at the argument of the given index. If
 * +maxlen+ is smaller than zero, which is the default, pinktrace tries to
 * determine the length of the string itself.
 */
VALUE
pinkrb_decode_string(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	int maxlen;
	char *str;
	VALUE vpid, vind, vmax, vbit, vret;

	switch (rb_scan_args(argc, argv, "22", &vpid, &vind, &vmax, &vbit)) {
	case 2:
		maxlen = -1;
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 3:
		maxlen = NUM2INT(vmax);
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 4:
		maxlen = NUM2INT(vmax);
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);
	ind = FIX2UINT(vind);

	if (maxlen < 0) {
		/* Use pink_decode_string_persistent() */
		str = pink_decode_string_persistent(pid, bit, ind);
		if (!str)
			rb_sys_fail("pink_decode_string_persistent()");

		vret = rb_str_new2(str);
		free(str);
		return vret;
	}

	/* Use pink_decode_string() */
	str = ALLOC_N(char, maxlen);
	if (!pink_decode_string(pid, bit, ind, str, maxlen))
		rb_sys_fail("pink_decode_string()");

	vret = rb_str_new2(str);
	if (str)
		free(str);
	return vret;
}

/*
 * Document-method: PinkTrace::String.encode
 * call-seq:
 *   PinkTrace::String.encode(pid, index, str, [bitness=PinkTrace::Bitness::DEFAULT]) -> nil
 *
 * Encode a string into the argument of the given index safely.
 *
 * Availability: Linux
 */
VALUE
pinkrb_encode_string_safe(int argc, VALUE *argv, VALUE mod)
{
#ifdef PINKTRACE_LINUX
	pid_t pid;
	unsigned bit, ind;
	size_t len;
	char *src;
	VALUE vpid, vind, vsrc, vbit;

	switch (rb_scan_args(argc, argv, "31", &vpid, &vind, &vsrc, &vbit)) {
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

	SafeStringValue(vsrc);
	src = RSTRING_PTR(vsrc);
	len = RSTRING_LEN(vsrc);

	if (!pink_encode_simple_safe(pid, bit, ind, src, ++len))
		rb_sys_fail("pink_encode_simple_safe()");

	return Qnil;
#else
	rb_raise(rb_eNotImpError, "Not implemented");
#endif
}

/*
 * Document-method: PinkTrace::String.encode!
 * call-seq:
 *   PinkTrace::String.encode!(pid, index, str, [bitness=PinkTrace::Bitness::DEFAULT]) -> nil
 *
 * Encode a string into the argument of the given index.
 */
VALUE
pinkrb_encode_string(int argc, VALUE *argv, VALUE mod)
{
	pid_t pid;
	unsigned bit, ind;
	size_t len;
	char *src;
	VALUE vpid, vind, vsrc, vbit;

	switch (rb_scan_args(argc, argv, "31", &vpid, &vind, &vsrc, &vbit)) {
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

	SafeStringValue(vsrc);
	src = RSTRING_PTR(vsrc);
	len = RSTRING_LEN(vsrc);

	if (!pink_encode_simple(pid, bit, ind, src, ++len))
		rb_sys_fail("pink_encode_simple()");

	return Qnil;
}
