SUBDIRS= .

COMMON_CFLAGS= \
	       -I$(top_srcdir)/include \
	       -L$(top_builddir)/src/.libs -L$(top_builddir)/src/easy/.libs \
	       @PINKTRACE_CFLAGS@
COMMON_LINK= \
	     -lpinktrace_@PINKTRACE_PC_SLOT@ \
	     -lpinktrace_easy_@PINKTRACE_PC_SLOT@

# Unit tests
test_SRCS= \
	   check_pinktrace_easy.h \
	   check_pinktrace_easy.c \
	   check_easy_process.c
EXTRA_DIST= $(test_SRCS)
if WANT_EASY
# Unit tests
TESTS= check_pinktrace_easy
check_PROGRAMS= check_pinktrace_easy
check_pinktrace_easy_SOURCES= $(test_SRCS)
check_pinktrace_easy_CFLAGS= $(COMMON_CFLAGS) $(check_CFLAGS)
check_pinktrace_easy_LDADD= $(COMMON_LINK) $(check_LIBS)
endif # WANT_EASY

# Program tests
t01_SRCS= \
	  t01-exit-genuine.c
EXTRA_DIST+= $(t01_SRCS)
if WANT_EASY
TESTS+= t01_exit_genuine
check_PROGRAMS+= t01_exit_genuine
t01_exit_genuine_SOURCES= $(t01_SRCS)
t01_exit_genuine_CFLAGS= $(COMMON_CFLAGS)
t01_exit_genuine_LDADD= $(COMMON_LINK)
endif # WANT_EASY

t02_SRCS= \
	  t02-exit-signal.c
EXTRA_DIST+= $(t02_SRCS)
if WANT_EASY
TESTS+= t02_exit_signal
check_PROGRAMS+= t02_exit_signal
t02_exit_signal_SOURCES= $(t02_SRCS)
t02_exit_signal_CFLAGS= $(COMMON_CFLAGS)
t02_exit_signal_LDADD= $(COMMON_LINK)
endif # WANT_EASY
