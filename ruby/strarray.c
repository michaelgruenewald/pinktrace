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
 * Document-method: PinkTrace::StringArray.decode
 * call-seq:
 *   PinkTrace::StringArray.decode(pid, arg, index, [[maxlen=-1], [bitness=PinkTrace::Bitness::DEFAULT]]) -> String or nil
 *
 * This function decodes the member of the string array pointed by the address
 * +arg+. The +index+ argument specifies the index of the member in the string
 * array.
 *
 * Note: If the string array member was NULL, this function returns nil.
 */
VALUE
pinkrb_decode_strarray(int argc, VALUE *argv, VALUE mod)
{
	bool nil;
	pid_t pid;
	unsigned bit, ind;
	long arg;
	int maxlen;
	char *str;
	VALUE vpid, varg, vind, vmax, vbit, vret;

	switch (rb_scan_args(argc, argv, "32", &vpid, &varg, &vind, &vmax, &vbit)) {
	case 3:
		maxlen = -1;
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 4:
		maxlen = NUM2INT(vmax);
		bit = PINKTRACE_BITNESS_DEFAULT;
		break;
	case 5:
		maxlen = NUM2INT(vmax);
		bit = FIX2UINT(vbit);
		break;
	default:
		abort();
	}
	pid = NUM2PIDT(vpid);
	arg = NUM2LONG(varg);
	ind = FIX2UINT(vind);

	if (maxlen < 0) {
		/* Use pink_decode_string_array_member_persistent() */
		errno = 0;
		str = pink_decode_string_array_member_persistent(pid, bit, arg, ind);
		if (!str) {
			if (errno)
				rb_sys_fail("pink_decode_string_array_member_persistent()");
			return Qnil;
		}

		vret = rb_str_new2(str);
		free(str);
		return vret;
	}

	/* Use pink_decode_string_array_member() */
	str = ALLOC_N(char, maxlen);
	if (!pink_decode_string_array_member(pid, bit, arg, ind, str, maxlen, &nil))
		rb_sys_fail("pink_decode_string_array_member()");
	if (nil) {
		free(str);
		return Qnil;
	}

	vret = rb_str_new2(str);
	if (str)
		free(str);
	return vret;
}
