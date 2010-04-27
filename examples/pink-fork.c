/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <pinktrace/pink.h>

int
main(void)
{
	pid_t pid;
	pink_error_t error;

	/* Fork and start tracing. */
	if ((pid = pink_fork(PINK_TRACE_OPTION_ALL, &error)) < 0) {
		fprintf(stderr, "pink_fork: %s, %s\n",
			pink_error_tostring(error),
			strerror(errno));
		return EXIT_FAILURE;
	}
	else if (!pid) /* child */
		pause();
	else {
		/* At this point child has been stopped for tracing and stopped
		 * itself using SIGSTOP. We don't do anything interesting for
		 * this example. */

		/* Kill the child */
		pink_trace_kill(pid);
		return EXIT_SUCCESS;
	}
}
