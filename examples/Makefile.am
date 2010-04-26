EXAMPLES= pink-about pink-fork pink-example
AM_CFLAGS= -I$(top_srcdir)/include -L$(top_builddir)/src/.libs @PINKTRACE_CFLAGS@
COMMON_LINK= -lpinktrace_@PINKTRACE_PC_SLOT@

pink_about_SOURCES= pink-about.c
pink_about_LDADD= $(COMMON_LINK)

pink_fork_SOURCES= pink-fork.c
pink_fork_LDADD= $(COMMON_LINK)

pink_example_SOURCES= pink-example.c
pink_example_LDADD= $(COMMON_LINK)

noinst_PROGRAMS= $(EXAMPLES)
