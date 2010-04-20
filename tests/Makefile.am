TESTS= check_libpinktrace
check_PROGRAMS= check_libpinktrace
check_libpinktrace_SOURCES= check_libpinktrace.c \
			    $(top_builddir)/src/pink.c
check_libpinktrace_CFLAGS= -I$(top_builddir)/src -L$(top_builddir)/src/.libs \
			   $(check_CFLAGS) @PINKTRACE_CFLAGS@
check_libpinktrace_LDADD= -lpinktrace $(check_LIBS)
