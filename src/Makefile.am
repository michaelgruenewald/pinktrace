DEFS+= -DGIT_HEAD=\"$(GIT_HEAD)\"
AM_CFLAGS= @PINKTRACE_CFLAGS@

lib_LTLIBRARIES= libpinktrace.la
libpinktrace_la_SOURCES= pink-trace.c
libpinktrace_la_LDFLAGS= -version-info $(LT_VERSION_INFO)

noinst_HEADERS= pinktrace-gcc.h pinktrace-internal.h
include_HEADERS= pinktrace.h
