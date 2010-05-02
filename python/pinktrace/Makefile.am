CLEANFILES= *.so
SUBDIRS= .
AM_CFLAGS= -I. -I@PYTHON_INCLUDE_DIR@ -I$(top_srcdir)/include \
	   @PINKTRACE_CFLAGS@

BUILT_SOURCES=

pythonlibdir= @PYTHON_INSTALL_DIR@/pinktrace
pythonlib_LTLIBRARIES=

BUILT_SOURCES+= about.c
about.c: about.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<
pythonlib_LTLIBRARIES+= about.la
about_la_SOURCES= about.c
about_la_LDFLAGS= -module -avoid-version -shared
about_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

BUILT_SOURCES+= bitness.c
bitness.c: bitness.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<
pythonlib_LTLIBRARIES+= bitness.la
bitness_la_SOURCES= bitness.c
bitness_la_LDFLAGS= -module -avoid-version -shared
bitness_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

BUILT_SOURCES+= event.c
event.c: event.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<
pythonlib_LTLIBRARIES+= event.la
event_la_SOURCES= event.c
event_la_LDFLAGS= -module -avoid-version -shared
event_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

BUILT_SOURCES+= fork.c
fork.c: fork.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<
pythonlib_LTLIBRARIES+= fork.la
fork_la_SOURCES= fork.c
fork_la_LDFLAGS= -module -avoid-version -shared
fork_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

BUILT_SOURCES+= string.c
string.c: string.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<
pythonlib_LTLIBRARIES+= string.la
string_la_SOURCES= string.c
string_la_LDFLAGS= -module -avoid-version -shared
string_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

BUILT_SOURCES+= syscall.c
syscall.c: syscall.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<
pythonlib_LTLIBRARIES+= syscall.la
syscall_la_SOURCES= syscall.c
syscall_la_LDFLAGS= -module -avoid-version -shared
syscall_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

BUILT_SOURCES+= trace.c
trace.c: trace.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<
pythonlib_LTLIBRARIES+= trace.la
trace_la_SOURCES= trace.c
trace_la_LDFLAGS= -module -avoid-version -shared
trace_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

# Hack to make tests work.
check_DATA=

check_DATA+= about.so
about.so: .libs/about.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

check_DATA+= bitness.so
bitness.so: .libs/bitness.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

check_DATA+= event.so
event.so: .libs/event.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

check_DATA+= fork.so
fork.so: .libs/fork.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

check_DATA+= string.so
string.so: .libs/string.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

check_DATA+= syscall.so
syscall.so: .libs/syscall.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

check_DATA+= trace.so
trace.so: .libs/trace.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

# Remove the .la files - *.la is never linked against (pinktrace is a module)
# and Python doesn't use libltdl. Note that the library gets installed by
# install-data, so that's where we need to hook.
install-data-hook:
	rm -f $(DESTDIR)$(pythonlibdir)/*.la
