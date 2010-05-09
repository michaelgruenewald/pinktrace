SUBDIRS= .

CLEANFILES=
AM_CFLAGS= -I$(top_srcdir)/include @PINKTRACE_CFLAGS@

libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES= \
					             pink-trace-internal.c
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_LDFLAGS= -version-info @VERSION_LIB_CURRENT@:@VERSION_LIB_REVISION@:0

lib_LTLIBRARIES = libpinktrace_ptrace_@PINKTRACE_PC_SLOT@.la

# Operating system specific sources
if FREEBSD
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-freebsd-trace.c \
						     pink-freebsd-util.c

# Platform specific sources
if I386
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-freebsd-trace-i386.c
endif # I386
if X86_64
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-freebsd-trace-amd64.c
endif # X86_64
endif # FREEBSD

if LINUX
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-linux-event.c \
					             pink-linux-trace.c \
					             pink-linux-util.c

# Platform specific sources
if I386
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-linux-trace-x86.c
endif
if X86_64
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-linux-trace-x86_64.c
endif
if IA64
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-linux-trace-ia64.c
endif
if POWERPC
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-linux-trace-powerpc.c
endif
if POWERPC64
libpinktrace_ptrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					             pink-linux-trace-powerpc.c
endif
endif # LINUX
