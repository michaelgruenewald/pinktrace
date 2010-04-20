TESTS= check_libpinktrace
check_PROGRAMS= check_libpinktrace
noinst_HEADERS= check_pinktrace.h
check_libpinktrace_SOURCES= main.c \
			    check_trace.c \
			    $(top_builddir)/src/pink-trace.c
check_libpinktrace_CFLAGS= -I$(top_srcdir)/include -L$(top_builddir)/src/.libs \
			   $(check_CFLAGS) @PINKTRACE_CFLAGS@
check_libpinktrace_LDADD= -lpinktrace $(check_LIBS)
