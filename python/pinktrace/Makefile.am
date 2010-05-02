CLEANFILES= about.so trace.so
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

check_DATA+= trace.so
trace.so: .libs/trace.so
	$(AM_V_GEN)
	$(AM_V_at)$(LN_S) $< $@

# Remove the .la files - *.la is never linked against (pinktrace is a module)
# and Python doesn't use libltdl. Note that the library gets installed by
# install-data, so that's where we need to hook.
install-data-hook:
	rm -f $(DESTDIR)$(pythonlibdir)/*.la
