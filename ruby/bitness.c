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
 * Document-method: PinkTrace::Bitness.get
 * call-seq:
 *   PinkTrace::Bitness.get(pid) -> fixnum
 *
 * Returns the bitness of the traced child.
 */
VALUE
pinkrb_bitness_get(VALUE mod, VALUE vpid)
{
	pid_t pid;
	int bit;

	pid = NUM2PIDT(vpid);
	bit = pink_bitness_get(pid);
	if (bit == PINK_BITNESS_UNKNOWN)
		rb_sys_fail("pink_bitness_get()");

	return INT2FIX(bit);
}

/*
 * Document-method: Pinktrace::Bitness.name
 * call-seq:
 *   PinkTrace::Bitness.name(bitness) -> String
 *
 * Returns the name of the given bitness.
 */
VALUE
pinkrb_bitness_name(VALUE mod, VALUE vbit)
{
	pink_bitness_t bit;

	bit = FIX2UINT(vbit);
	return rb_str_new2(pink_bitness_name(bit));
}

/*
 * Document-method: PinkTrace::Bitness.wordsize
 * call-seq:
 *   PinkTrace::Bitness.wordsize(bitness) -> Fixnum
 *
 * Returns the word size of the given bitness.
 */
VALUE
pinkrb_bitness_wordsize(VALUE mod, VALUE vbit)
{
	unsigned short wordsize;
	unsigned bit;

	bit = FIX2UINT(vbit);
	wordsize = pink_bitness_wordsize(bit);
	return INT2FIX(wordsize);
}
