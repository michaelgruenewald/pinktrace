SUBDIRS= .

TESTS= check_pinktrace_proc
check_PROGRAMS= check_pinktrace_proc
noinst_HEADERS= check_pinktrace_proc.h

check_pinktrace_proc_SOURCES= \
			      check_fs.c \
			      check_util.c \
			      main.c
check_pinktrace_proc_CFLAGS= \
			     -I$(top_srcdir)/include \
			     -L$(top_builddir)/src/backend/proc/.libs \
			     $(check_CFLAGS) @PINKTRACE_CFLAGS@
check_pinktrace_proc_LDADD= \
			    -lpinktrace_proc_@PINKTRACE_PC_SLOT@ \
			    $(check_LIBS)
