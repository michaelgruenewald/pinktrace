/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

#include <assert.h>
#include <err.h>
#include <errno.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <pinktrace/pink.h>

#define MAX_STRING_LEN 128

/* Utility functions */
static void
print_ret(long ret)
{
	if (ret >= 0)
		printf("= %ld", ret);
	else
		printf("= %ld (%s)", ret, strerror(-ret));
}

static void
print_open_flags(long flags)
{
	long aflags;
	bool found;

	found = true;

	/* Check out access flags */
	aflags = flags & 3;
	switch (aflags) {
	case O_RDONLY:
		printf("O_RDONLY");
		break;
	case O_WRONLY:
		printf("O_WRONLY");
		break;
	case O_RDWR:
		printf("O_RDWR");
		break;
	default:
		/* Nothing found */
		found = false;
	}

	if (flags & O_CREAT) {
		printf("%s | O_CREAT", found ? "" : "0");
		found = true;
	}

	if (!found)
		printf("%#x", (unsigned)flags);
}

/* A generic decoder for system calls. */
static void
decode_simple(pink_bitness_t bitness, long scno)
{
	const char *scname;

	/* Figure out the name of the system call. */
	scname = pink_name_syscall(scno, bitness);
	if (scname == NULL)
		printf("%ld", scno);
	else
		printf("%s", scname);
	printf("()");
}

/* A very basic decoder for open(2) system call. */
static void
decode_open(pid_t pid, pink_bitness_t bitness)
{
	long flags;
	char buf[MAX_STRING_LEN];

	if (!pink_decode_string(pid, bitness, 0, buf, MAX_STRING_LEN)) {
		perror("pink_decode_string");
		return;
	}
	if (!pink_util_get_arg(pid, bitness, 1, &flags)) {
		perror("pink_util_get_arg");
		return;
	}

	printf("open(\"%s\", ", buf);
	print_open_flags(flags);
	fputc(')', stdout);
}

int
main(int argc, char **argv)
{
	bool inexecve, insyscall;
	int sig, status, exit_code;
	long scno, ret;
	pid_t pid;
	pink_bitness_t bitness;

	/* Parse arguments */
	if (argc < 2) {
		fprintf(stderr, "Usage: %s program [argument...]\n", argv[0]);
		return EXIT_FAILURE;
	}

	/* Fork */
	if ((pid = fork()) < 0) {
		perror("fork");
		return EXIT_FAILURE;
	}
	else if (!pid) { /* child */
		/* Set up for tracing */
		if (!pink_trace_me()) {
			perror("pink_trace_me");
			_exit(EXIT_FAILURE);
		}
		/* Stop and let the parent continue the execution. */
		kill(getpid(), SIGSTOP);

		++argv;
		execvp(argv[0], argv);
		perror("execvp");
		_exit(-1);
	}
	else {
		waitpid(pid, &status, 0);
		assert(WIFSTOPPED(status));
		assert(WSTOPSIG(status) == SIGSTOP);

		/* Figure out the bitness of the traced child. */
		bitness = pink_bitness_get(pid);
		printf("Child %i runs in %s mode\n", pid, pink_bitness_tostring(bitness));

		inexecve = insyscall = false;
		sig = exit_code = 0;
		for (;;) {
			/* At this point the traced child is stopped and needs
			 * to be resumed.
			 */
			if (!pink_trace_syscall(pid, sig)) {
				perror("pink_trace_syscall");
				return (errno == ESRCH) ? 0 : 1;
			}
			sig = 0;

			/* Wait for the child */
			if ((pid = waitpid(pid, &status, 0)) < 0) {
				perror("waitpid");
				return (errno == ECHILD) ? 0 : 1;
			}

			/* Check the event */
			if (WIFSTOPPED(status)) {
				if (WSTOPSIG(status) == SIGTRAP) {
					/* We get this event twice, one at entering a
					 * system call and one at exiting a system
					 * call. */
					if (insyscall) {
						if (!pink_util_get_return(pid, &ret))
							err(EXIT_FAILURE, "pink_util_get_return");
						if (inexecve) {
							inexecve = false;
							if (!ret) {
								/* Update bitness */
								bitness = pink_bitness_get(pid);
								continue;
							}
						}
						/* Exiting the system call, print the
						 * return value. */
						fputc(' ', stdout);
						print_ret(ret);
						fputc('\n', stdout);
						insyscall = false;
					}
					else {
						/* Get the system call number and call
						 * the appropriate decoder. */
						if (!pink_util_get_syscall(pid, &scno))
							err(EXIT_FAILURE, "pink_util_get_syscall");
						if (scno == SYS_execve)
							inexecve = true;

						if (scno == SYS_open)
							decode_open(pid, bitness);
						else
							decode_simple(bitness, scno);
						insyscall = true;
					}
				}
				else {
					/* Child received a genuine signal.
					 * Send it to the child. */
					sig = WSTOPSIG(status);
				}
			}
			else if (WIFEXITED(status)) {
				exit_code = WEXITSTATUS(status);
				printf("Child %i exited normally with return code %d\n",
						pid, exit_code);
				break;
			}
			else if (WIFSIGNALED(status)) {
				exit_code = 128 + WTERMSIG(status);
				printf("Child %i exited with signal %d\n",
						pid, WTERMSIG(status));
				break;
			}
		}

		return exit_code;
	}
}
