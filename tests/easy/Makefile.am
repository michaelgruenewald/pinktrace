SUBDIRS= .

test_SRCS= \
	   check_pinktrace_easy.h \
	   check_easy_loop.c \
	   check_easy_process.c \
	   main.c
EXTRA_DIST= $(test_SRCS)

if WANT_EASY
TESTS= check_pinktrace_easy
check_PROGRAMS= check_pinktrace_easy
check_pinktrace_easy_SOURCES= $(test_SRCS)

check_pinktrace_easy_CFLAGS= \
			     -I$(top_srcdir)/include \
			     -L$(top_builddir)/src/.libs -L$(top_builddir)/src/easy/.libs \
			     $(check_CFLAGS) \
			     @PINKTRACE_CFLAGS@
check_pinktrace_easy_LDADD= \
			    -lpinktrace_@PINKTRACE_PC_SLOT@ \
			    -lpinktrace_easy_@PINKTRACE_PC_SLOT@ \
			    $(check_LIBS)
endif # WANT_EASY
