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
	pink_context_t *ctx;

	/* Create a tracing context. */
	ctx = pink_context_new();
	if (!ctx) {
		perror("pink_context_new");
		return EXIT_FAILURE;
	}

	/* Set tracing options */
	pink_context_set_options(ctx, PINK_TRACE_OPTION_EXEC);

	if ((pid = pink_fork(ctx)) < 0) {
		fprintf(stderr, "pink_fork: %s, %s\n",
			pink_error_tostring(pink_context_get_error(ctx)),
			strerror(errno));
		pink_context_free(ctx);
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

		/* Cleanup and exit */
		pink_context_free(ctx);
		return EXIT_SUCCESS;
	}
}
