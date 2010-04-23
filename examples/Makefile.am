EXAMPLES= pink-example

pink_example_SOURCES= pink-example.c
pink_example_CFLAGS= -I$(top_srcdir)/include -L$(top_builddir)/src/.libs @PINKTRACE_CFLAGS@
pink_example_LDADD= -lpinktrace

noinst_PROGRAMS= $(EXAMPLES)
