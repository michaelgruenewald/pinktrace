/**
 * \file
 *
 * Example \ref pink-fork-freebsd.c "pink-fork-freebsd.c" .
 **/

/**
 * \example pink-fork-freebsd.c
 *
 * Example demonstrating how to fork and start tracing a process on FreeBSD.
 **/

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#include <pinktrace/pink.h>

int
main(void)
{
	int status;
	pid_t pid;

	/* Fork and start tracing. */
	if ((pid = fork()) < 0) {
		perror("fork");
		return EXIT_FAILURE;
	}
	else if (!pid) {
		/* Set up for tracing. */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		/* Stop, so that the parent has the chance to set up options
		 * for tracing. */
		kill(getpid(), SIGSTOP);

		/* Here's the point where you can do whatever the traced child
		 * should do. */
		printf("hello world\n");

		_exit(EXIT_SUCCESS);
	}
	else {
		waitpid(pid, &status, 0);
		assert(WIFSTOPPED(status));
		assert(WSTOPSIG(status) == SIGSTOP);

		/* The child is ready for tracing, nothing interesting in this
		 * example, let the child resume its execution. */
		pink_trace_resume(pid, 0);

		/* Wait for the child to exit. */
		waitpid(pid, &status, 0);

		return EXIT_SUCCESS;
	}
}
