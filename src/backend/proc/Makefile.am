SUBDIRS= .

CLEANFILES=
AM_CFLAGS= -I$(top_srcdir)/include @PINKTRACE_CFLAGS@

libpinktrace_proc_@PINKTRACE_PC_SLOT@_la_SOURCES= \
						  pink-proc-fs.c
libpinktrace_proc_@PINKTRACE_PC_SLOT@_la_LDFLAGS= -version-info @VERSION_LIB_CURRENT@:@VERSION_LIB_REVISION@:0

lib_LTLIBRARIES= libpinktrace_proc_@PINKTRACE_PC_SLOT@.la
