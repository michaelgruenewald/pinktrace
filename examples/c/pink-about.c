/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pinktrace/pink.h>

int
main(void)
{
	printf("Built using pinktrace %d.%d.%d%s",
		PINKTRACE_VERSION_MAJOR,
		PINKTRACE_VERSION_MINOR,
		PINKTRACE_VERSION_MICRO,
		PINKTRACE_VERSION_SUFFIX);

	if (strncmp(PINKTRACE_GIT_HEAD, "", 1))
		printf(" %s", PINKTRACE_GIT_HEAD);

	fputc('\n', stdout);

	return EXIT_SUCCESS;
}
