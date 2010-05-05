TESTS= check_libpinktrace
check_PROGRAMS= check_libpinktrace
noinst_HEADERS= check_pinktrace.h
check_libpinktrace_SOURCES= main.c \
			    check_bitness.c \
			    check_decode.c \
			    check_encode.c \
			    check_event.c \
			    check_trace.c \
			    check_util.c
check_libpinktrace_CFLAGS= -I$(top_srcdir)/include -L$(top_builddir)/src/.libs \
			   $(check_CFLAGS) @PINKTRACE_CFLAGS@
check_libpinktrace_LDADD= -lpinktrace_@PINKTRACE_PC_SLOT@ $(check_LIBS)
