DEFS+= -DGIT_HEAD=\"$(GIT_HEAD)\"
AM_CFLAGS= -I$(top_srcdir)/include @PINKTRACE_CFLAGS@

lib_LTLIBRARIES= libpinktrace.la
libpinktrace_la_SOURCES= pink-context.c pink-error.c \
			 pink-event.c pink-fork.c \
			 pink-loop.c pink-trace.c
libpinktrace_la_LDFLAGS= -version-info $(LT_VERSION_INFO)
