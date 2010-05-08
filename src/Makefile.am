CLEANFILES=
AM_CFLAGS= -I$(top_srcdir)/include @PINKTRACE_CFLAGS@

libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES= \
					     pink-bitness.c \
					     pink-name-wrap.c \
					     pink-trace-internal.c
libpinktrace_@PINKTRACE_PC_SLOT@_la_LDFLAGS= -version-info @VERSION_LIB_CURRENT@:@VERSION_LIB_REVISION@:0

lib_LTLIBRARIES = libpinktrace_@PINKTRACE_PC_SLOT@.la
noinst_HEADERS= \
		pink-linux-syscallent-x86.h \
		pink-linux-syscallent-x86_64.h \
		pink-linux-syscallent-ia64.h \
		pink-linux-syscallent-powerpc.h

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
endif
if X86_64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-x86_64.c \
					      pink-linux-trace-x86_64.c
endif
if IA64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-ia64.c \
					      pink-linux-trace-ia64.c
endif
if POWERPC
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-powerpc.c \
					      pink-linux-trace-powerpc.c
endif
if POWERPC64
libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
					      pink-linux-name-powerpc.c \
					      pink-linux-trace-powerpc.c
endif
endif # LINUX

# Generated sources
nodist_libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES=
BUILT_SOURCES=

if FREEBSD
nodist_libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= pink-freebsd-syscallent.h
BUILT_SOURCES+= pink-freebsd-syscallent.h
CLEANFILES+= syscalls.master pink-freebsd-syscallent.h

syscalls.master: $(SYSSRC)/kern/syscalls.master
	$(AM_V_GEN)
	$(AM_V_at)cat $< > $@
pink-freebsd-syscallent.h: syscalls.master
	$(AM_V_GEN)
	$(AM_V_at)/bin/sh $(SYSSRC)/kern/makesyscalls.sh $< pink-freebsd-i386.conf

if X86_64
nodist_libpinktrace_@PINKTRACE_PC_SLOT@_la_SOURCES+= pink-freebsd-syscallent32.h
BUILT_SOURCES+= pink-freebsd-syscallent32.h
CLEANFILES+= fbsd32-syscalls.master pink-freebsd-syscallent32.h

fbsd32-syscalls.master: $(SYSSRC)/compat/freebsd32/syscalls.master
	$(AM_V_GEN)
	$(AM_V_at)cat $< > $@
pink-freebsd-syscallent32.h: fbsd32-syscalls.master
	$(AM_V_GEN)
	$(AM_V_at)/bin/sh $(SYSSRC)/kern/makesyscalls.sh $< pink-freebsd-fbsd32.conf
endif # X86_64
endif # FREEBSD
