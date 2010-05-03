CLEANFILES= *.so *.pyc
SUBDIRS= .
AM_CFLAGS= -I. -I@PYTHON_INCLUDE_DIR@ -I$(top_srcdir)/include \
	   @PINKTRACE_CFLAGS@
BUILT_SOURCES=

noinst_HEADERS= pink-python-hacks.h

pythonlibdir= @PYTHON_INSTALL_DIR@/pinktrace
pythonlib_LTLIBRARIES=

pythonlib_LTLIBRARIES+= about.la
about_la_SOURCES= pink-python-about.c
about_la_LDFLAGS= -module -avoid-version -shared
about_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= bitness.la
bitness_la_SOURCES= pink-python-bitness.c
bitness_la_LDFLAGS= -module -avoid-version -shared
bitness_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= event.la
event_la_SOURCES= pink-python-event.c
event_la_LDFLAGS= -module -avoid-version -shared
event_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

pythonlib_LTLIBRARIES+= fork.la
fork_la_SOURCES= pink-python-fork.c
fork_la_LDFLAGS= -module -avoid-version -shared
fork_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

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

check_DATA+= socket.so
socket.so: .libs/socket.so
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
