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
 * Document-method: PinkTrace::Event.decide
 * call-seq:
 *   PinkTrace::Event.decide([status=$?.status]) -> fixnum
 *
 * Returns the last event made by child.
 */
VALUE
pinkrb_event_decide(int argc, VALUE *argv, VALUE mod)
{
	unsigned int event;
	int status;
	VALUE vstatus, ls;

	switch (rb_scan_args(argc, argv, "01", &vstatus)) {
	case 0:
		/* Use $?.status */
#ifdef HAVE_RB_LAST_STATUS_GET /* ruby-1.9 */
		ls = rb_last_status_get();
#else /* ruby-1.8 */
		ls = rb_gv_get("$?");
#endif /* HAVE_RB_LAST_STATUS_GET */
		if (NIL_P(ls))
			rb_raise(rb_eTypeError, "$? is nil");
		status = NUM2INT(rb_iv_get(ls, "status"));
		break;
	case 1:
		status = NUM2INT(vstatus);
		break;
	default:
		abort();
	}

	event = pink_event_decide(status);
	return UINT2NUM(event);
}
