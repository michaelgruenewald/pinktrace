SUBDIRS= .

COMMON_CFLAGS= \
	       -I$(top_srcdir)/include \
	       -L$(top_builddir)/src/.libs -L$(top_builddir)/src/easy/.libs \
	       @PINKTRACE_CFLAGS@
COMMON_LINK= \
	     -lpinktrace_@PINKTRACE_PC_SLOT@ \
	     -lpinktrace_easy_@PINKTRACE_PC_SLOT@

# Program tests
t01_SRCS= \
	  t01-exit-genuine.c
EXTRA_DIST= $(t01_SRCS)
if WANT_EASY
TESTS= t01_exit_genuine
check_PROGRAMS= t01_exit_genuine
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

t03_SRCS= \
	  t03-signal.c
EXTRA_DIST+= $(t03_SRCS)
if WANT_EASY
TESTS+= t03_signal
check_PROGRAMS+= t03_signal
t03_signal_SOURCES= $(t03_SRCS)
t03_signal_CFLAGS= $(COMMON_CFLAGS)
t03_signal_LDADD= $(COMMON_LINK)
endif # WANT_EASY

t04_SRCS= \
	  t04-pre-exit.c
EXTRA_DIST+= $(t04_SRCS)
if WANT_EASY
TESTS+= t04_pre_exit
check_PROGRAMS+= t04_pre_exit
t04_pre_exit_SOURCES= $(t04_SRCS)
t04_pre_exit_CFLAGS= $(COMMON_CFLAGS)
t04_pre_exit_LDADD= $(COMMON_LINK)
endif # WANT_EASY

t05_SRCS= \
	  t05-pre-exit-signal.c
EXTRA_DIST+= $(t05_SRCS)
if WANT_EASY
TESTS+= t05_pre_exit_signal
check_PROGRAMS+= t05_pre_exit_signal
t05_pre_exit_signal_SOURCES= $(t05_SRCS)
t05_pre_exit_signal_CFLAGS= $(COMMON_CFLAGS)
t05_pre_exit_signal_LDADD= $(COMMON_LINK)
endif # WANT_EASY
