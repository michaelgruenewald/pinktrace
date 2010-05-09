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

#include "check_pinktrace_proc.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <check.h>

int
main(void)
{
	int number_failed;
	SRunner *sr;

	/* Check if /proc is mounted */
	if (access("/proc/curproc", R_OK) < 0) {
		perror("access(\"/proc/curproc\", R_OK)");
		fprintf(stderr, "Is /proc mounted?\n");
		return EXIT_FAILURE;
	}

	/* Add suites */
	sr = srunner_create(fs_suite_create());

	/* Run and grab the results */
	srunner_run_all(sr, CK_VERBOSE);
	number_failed = srunner_ntests_failed(sr);

	/* Cleanup and exit */
	srunner_free(sr);
	return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
