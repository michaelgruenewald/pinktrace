CLEANFILES= *.so *.pyc
SUBDIRS= .
AM_CFLAGS= -I. -I@PYTHON_INCLUDE_DIR@ -I$(top_srcdir)/include \
	   @PINKTRACE_CFLAGS@
BUILT_SOURCES=

noinst_HEADERS= pink-python-hacks.h

pythonlibdir= @PYTHON_INSTALL_DIR@/pinktrace
pythonlib_LTLIBRARIES=

pythonlib_DATA= __init__.py

if WANT_IPV6
__init__.py: __init__.py.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) \
		-e "s,##VERSION##,$(VERSION)," \
		-e "s,##VERSION_MAJOR##,$(VERSION_MAJOR)," \
		-e "s,##VERSION_MINOR##,$(VERSION_MINOR)," \
		-e "s,##VERSION_MICRO##,$(VERSION_MICRO)," \
		-e "s,##VERSION_SUFFIX##,$(VERSION_SUFFIX)," \
		-e "s,##GIT_HEAD##,$(GITHEAD)," \
		-e "s,##HAVE_IPV6##,True," \
		$< > $@
else
__init__.py: __init__.py.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) \
		-e "s,##VERSION##,$(VERSION)," \
		-e "s,##VERSION_MAJOR##,$(VERSION_MAJOR)," \
		-e "s,##VERSION_MINOR##,$(VERSION_MINOR)," \
		-e "s,##VERSION_MICRO##,$(VERSION_MICRO)," \
		-e "s,##VERSION_SUFFIX##,$(VERSION_SUFFIX)," \
		-e "s,##GIT_HEAD##,$(GITHEAD)," \
		-e "s,##HAVE_IPV6##,False," \
		$< > $@
endif

pythonlib_LTLIBRARIES+= bitness.la
bitness_la_SOURCES= pink-python-bitness.c
bitness_la_LDFLAGS= -module -avoid-version -shared
bitness_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= event.la
event_la_SOURCES= pink-python-event.c
event_la_LDFLAGS= -module -avoid-version -shared
event_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= socket.la
socket_la_SOURCES= pink-python-socket.c
socket_la_LDFLAGS= -module -avoid-version -shared
socket_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= string.la
string_la_SOURCES= pink-python-string.c
string_la_LDFLAGS= -module -avoid-version -shared
string_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= syscall.la
syscall_la_SOURCES= pink-python-syscall.c
syscall_la_LDFLAGS= -module -avoid-version -shared
syscall_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= trace.la
trace_la_SOURCES= pink-python-trace.c
trace_la_LDFLAGS= -module -avoid-version -shared
trace_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

# Hack to make tests work.
check_DATA= bitness.so \
	    event.so \
	    socket.so \
	    string.so \
	    syscall.so \
	    trace.so

%.so: .libs/%.so
	$(AM_V_GEN)
	$(AM_V_at)test -h $@ || $(LN_S) $< $@

# Remove the .la files - *.la is never linked against (pinktrace is a module)
# and Python doesn't use libltdl. Note that the library gets installed by
# install-data, so that's where we need to hook.
install-data-hook:
	rm -f $(DESTDIR)$(pythonlibdir)/*.la
