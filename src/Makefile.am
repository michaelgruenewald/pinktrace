DEFS+= -DGIT_HEAD=\"$(GIT_HEAD)\"
AM_CFLAGS= -I$(top_srcdir)/include @PINKTRACE_CFLAGS@

lib_LTLIBRARIES= libpinktrace.la
libpinktrace_la_SOURCES= pink-bitness.c pink-context.c \
			 pink-error.c pink-event.c \
			 pink-fork.c pink-loop.c \
			 pink-trace.c
libpinktrace_la_LDFLAGS= -version-info $(LT_VERSION_INFO)

# Platform specific sources
if I386
libpinktrace_la_SOURCES+= pink-trace-x86.c
endif
if X86_64
libpinktrace_la_SOURCES+= pink-trace-x86_64.c
endif
if IA64
libpinktrace_la_SOURCES+= pink-trace-ia64.c
endif
if POWERPC
libpinktrace_la_SOURCES+= pink-trace-powerpc.c
endif
if POWERPC64
libpinktrace_la_SOURCES+= pink-trace-powerpc.c
endif
