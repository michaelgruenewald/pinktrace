SUBDIRS= backend
if FREEBSD
SUBDIRS+= freebsd
endif
if LINUX
SUBDIRS+= linux
endif
SUBDIRS+= .

CLEANFILES=
AM_CFLAGS= -I$(top_srcdir)/include @PINKTRACE_CFLAGS@

libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES= \
					     pink-bitness.c \
					     pink-name-wrap.c
libpinktrace_@PINKTRACE_PC_SLOT@_la_LDFLAGS= -version-info @VERSION_LIB_CURRENT@:@VERSION_LIB_REVISION@:0
libpinktrace_@PINKTRACE_PC_SLOT@_la_LIBADD=
if FREEBSD
# libpinktrace_@PINKTRACE_PC_SLOT@_la_LIBADD+= backend/proc/libpinktrace_proc_@PINKTRACE_PC_SLOT@.la
libpinktrace_@PINKTRACE_PC_SLOT@_la_LIBADD+= backend/ptrace/libpinktrace_ptrace_@PINKTRACE_PC_SLOT@.la
endif # FREEBSD
if LINUX
libpinktrace_@PINKTRACE_PC_SLOT@_la_LIBADD+= backend/ptrace/libpinktrace_ptrace_@PINKTRACE_PC_SLOT@.la
endif # LINUX

lib_LTLIBRARIES = libpinktrace_@PINKTRACE_PC_SLOT@.la

# Operating system specific sources
if FREEBSD
# Platform specific sources
if I386
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-freebsd-name-i386.c
endif # I386
if X86_64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-freebsd-name-amd64.c
endif # X86_64
endif # FREEBSD

if LINUX
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-socket.c

# Platform specific sources
if I386
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-x86.c
endif # I386
if X86_64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-x86_64.c
endif # X86_64
if IA64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-ia64.c
endif # IA64
if POWERPC
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-powerpc.c
endif # POWERPC
if POWERPC64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-powerpc.c
endif # POWERPC
endif # LINUX
