EXAMPLES= pink-lowlevel

pink_lowlevel_SOURCES= pink-low.c
pink_lowlevel_CFLAGS= -I$(top_srcdir)/include -L$(top_builddir)/src/.libs @PINKTRACE_CFLAGS@
pink_lowlevel_LDADD= -lpinktrace

noinst_PROGRAMS= $(EXAMPLES)
