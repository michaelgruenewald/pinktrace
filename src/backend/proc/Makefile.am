SUBDIRS= .

CLEANFILES=
AM_CFLAGS= -I$(top_srcdir)/include @PINKTRACE_CFLAGS@

libpinktrace_proc_@PINKTRACE_PC_SLOT@_la_SOURCES= \
						  pink-fs.c \
						  pink-util.c
libpinktrace_proc_@PINKTRACE_PC_SLOT@_la_LDFLAGS= -version-info @VERSION_LIB_CURRENT@:@VERSION_LIB_REVISION@:0

# Platform specific sources
if I386
libpinktrace_proc_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
						   pink-util-i386.c
endif # I386
if X86_64
libpinktrace_proc_@PINKTRACE_PC_SLOT@_la_SOURCES+= \
						   pink-util-amd64.c
endif # X86_64

lib_LTLIBRARIES= libpinktrace_proc_@PINKTRACE_PC_SLOT@.la
