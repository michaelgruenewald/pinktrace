TESTS= check_pinktrace
check_PROGRAMS= check_pinktrace
noinst_HEADERS= check_pinktrace.h
check_pinktrace_SOURCES= main.c
if FREEBSD
check_pinktrace_SOURCES+= \
			  check_freebsd_trace.c \
			  check_freebsd_util.c \
			  check_freebsd_decode.c \
			  check_freebsd_encode.c
endif # FREEBSD

if LINUX
check_pinktrace_SOURCES+= \
			  check_linux_trace.c \
			  check_linux_util.c \
			  check_linux_decode.c \
			  check_linux_encode.c \
			  check_bitness.c \
			  check_event.c
endif # LINUX

check_pinktrace_CFLAGS= -I$(top_srcdir)/include -L$(top_builddir)/src/.libs \
			$(check_CFLAGS) @PINKTRACE_CFLAGS@
check_pinktrace_LDADD= -lpinktrace_@PINKTRACE_PC_SLOT@ $(check_LIBS)
