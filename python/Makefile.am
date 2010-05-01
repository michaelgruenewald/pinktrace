IF_PYTHON_SOURCES= pinktrace.pyx
EXTRA_DIST= $(IF_PYTHON_SOURCES)

if ENABLE_PYTHON
AM_CFLAGS= -I. -I@PYTHON_INCLUDE_DIR@ -I$(top_srcdir)/include \
	   @PINKTRACE_CFLAGS@

BUILT_SOURCES= pinktrace.c
pinktrace.c: $(IF_PYTHON_SOURCES)
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) -o $@ $<

pythonlibdir= @PYTHON_INSTALL_DIR@
pythonlib_LTLIBRARIES= pinktrace.la

pinktrace_la_SOURCES= pinktrace.c
pinktrace_la_LDFLAGS= -module -avoid-version -shared
pinktrace_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

# Remove the .la file - pinktrace.la is never linked against (it's a module)
# and Python doesn't use libltdl. Note that the library gets installed by
# install-data, so that's where we need to hook.
install-data-hook:
	rm -f $(DESTDIR)$(pythonlibdir)/pinktrace.la
endif
