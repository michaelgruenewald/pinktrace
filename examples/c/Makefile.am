SUBDIRS= .
EXAMPLES= \
	  pink-about \
	  pink-fork \
	  pink-simple-strace
AM_CFLAGS= \
	   -I$(top_srcdir)/include -L$(top_builddir)/src/.libs \
	   @PINKTRACE_CFLAGS@
COMMON_LINK= -lpinktrace_@PINKTRACE_PC_SLOT@

pink_about_SOURCES= pink-about.c
pink_about_LDADD= $(COMMON_LINK)

pink_fork_SOURCES=
if FREEBSD
pink_fork_SOURCES+= pink-fork-freebsd.c
endif # FREEBSD
if LINUX
pink_fork_SOURCES+= pink-fork-linux.c
endif # LINUX
pink_fork_LDADD= $(COMMON_LINK)

pink_simple_strace_SOURCES=
if FREEBSD
pink_simple_strace_SOURCES+= pink-simple-strace-freebsd.c
endif # FREEBSD
if LINUX
pink_simple_strace_SOURCES+= pink-simple-strace-linux.c
endif # LINUX
pink_simple_strace_LDADD= $(COMMON_LINK)

noinst_PROGRAMS= $(EXAMPLES)
