TESTS= check_pinktrace_ptrace
check_PROGRAMS= check_pinktrace_ptrace
noinst_HEADERS= check_pinktrace_ptrace.h

check_pinktrace_ptrace_SOURCES= \
				check_bitness.c \
				main.c
if FREEBSD
check_pinktrace_ptrace_SOURCES+= \
				 check_freebsd_trace.c \
				 check_freebsd_util.c \
				 check_freebsd_decode.c \
				 check_freebsd_encode.c
endif # FREEBSD

if LINUX
check_pinktrace_ptrace_SOURCES+= \
				 check_linux_trace.c \
				 check_linux_util.c \
				 check_linux_decode.c \
				 check_linux_encode.c \
				 check_linux_event.c
endif # LINUX

check_pinktrace_ptrace_CFLAGS= \
			       -I$(top_srcdir)/include \
			       -L$(top_builddir)/src/backend/ptrace/.libs \
			       $(check_CFLAGS) @PINKTRACE_CFLAGS@
check_pinktrace_ptrace_LDADD= \
			      -lpinktrace_ptrace_@PINKTRACE_PC_SLOT@ \
			      $(check_LIBS)
