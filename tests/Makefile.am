SUBDIRS= . easy
EXTRA_DIST=

IF_CHECK_FREEBSD_SRCS= \
		       check_freebsd_trace.c \
		       check_freebsd_util.c \
		       check_freebsd_decode.c \
		       check_freebsd_encode.c

IF_CHECK_LINUX_SRCS= \
		     check_linux_trace.c \
		     check_linux_util.c \
		     check_linux_decode.c \
		     check_linux_encode.c \
		     check_linux_event.c

IF_CHECK_SRCS= \
	       check_bitness.c \
	       main.c

noinst_HEADERS= check_pinktrace.h
EXTRA_DIST+= $(IF_CHECK_FREEBSD_SRCS) $(IF_CHECK_LINUX_SRCS) $(IF_CHECK_SRCS)

if FREEBSD
IF_CHECK_SRCS+= $(IF_CHECK_FREEBSD_SRCS)
endif # FREEBSD

if LINUX
IF_CHECK_SRCS+= $(IF_CHECK_LINUX_SRCS)
endif # LINUX

if HAVE_LIBCHECK
TESTS= check_pinktrace
check_PROGRAMS= check_pinktrace

check_pinktrace_SOURCES= \
			 $(IF_CHECK_SRCS)
check_pinktrace_CFLAGS= \
			-I$(top_srcdir)/include -L$(top_builddir)/src/.libs \
			$(check_CFLAGS) \
			@PINKTRACE_CFLAGS@
check_pinktrace_LDADD= \
		       -lpinktrace_@PINKTRACE_PC_SLOT@ \
		       $(check_LIBS)
endif # HAVE_LIBCHECK
