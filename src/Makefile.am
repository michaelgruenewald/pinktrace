SUBDIRS=
if FREEBSD
SUBDIRS+= freebsd
endif # FREEBSD

if LINUX
SUBDIRS+= linux
endif # LINUX
SUBDIRS+= .

CLEANFILES=
AM_CFLAGS= \
	   -I$(top_srcdir)/include \
	   @PINKTRACE_CFLAGS@

lib_LTLIBRARIES = libpinktrace_@PINKTRACE_PC_SLOT@.la

libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES= \
					     pink-bitness.c \
					     pink-trace-internal.c
libpinktrace_@PINKTRACE_PC_SLOT@_la_LDFLAGS= -version-info @VERSION_LIB_CURRENT@:@VERSION_LIB_REVISION@:0

# Operating system specific sources
if FREEBSD
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-freebsd-trace.c \
					      pink-freebsd-util.c

# Platform specific sources
if I386
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-freebsd-name-i386.c \
					      pink-freebsd-trace-i386.c
endif # I386
if X86_64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-freebsd-name-amd64.c \
					      pink-freebsd-trace-amd64.c
endif # X86_64
endif # FREEBSD

if LINUX
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-event.c \
					      pink-linux-socket.c \
					      pink-linux-trace.c \
					      pink-linux-util.c

# Platform specific sources
if I386
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-x86.c \
					      pink-linux-trace-x86.c
endif # I386

if X86_64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-x86_64.c \
					      pink-linux-trace-x86_64.c
endif # X86_64

if IA64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-ia64.c \
					      pink-linux-trace-ia64.c
endif # IA64

if POWERPC
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-powerpc.c \
					      pink-linux-trace-powerpc.c
endif # POWERPC

if POWERPC64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-powerpc.c \
					      pink-linux-trace-powerpc.c
endif # POWERPC64

if ARM
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-arm.c \
					      pink-linux-trace-arm.c
endif # ARM

endif # LINUX
