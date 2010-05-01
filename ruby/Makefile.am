IF_RUBY_SOURCES= pink-ruby.c
EXTRA_DIST= $(IF_RUBY_SOURCES)

if ENABLE_RUBY
AM_CFLAGS= -I. -I@RUBY_HDRDIR@ -I@RUBY_HDRARCHDIR@ -I$(top_srcdir)/include \
	   @PINKTRACE_CFLAGS@ @PINKTRACE_RUBY_CFLAGS@

rubylibdir= @RUBY_INSTALL_DIR@
rubylib_LTLIBRARIES= PinkTrace.la

PinkTrace_la_SOURCES= $(IF_RUBY_SOURCES)
PinkTrace_la_LDFLAGS= -module -avoid-version -shared
PinkTrace_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

TESTS= TEST_01_trace.rb TEST_02_fork.rb \
       TEST_03_event.rb TEST_04_bitness.rb \
       TEST_05_syscall.rb TEST_06_string.rb \
       TEST_07_socket.rb

# Remove the .la file - PinkTrace.la is never linked against (it's a module)
# and Ruby doesn't use libltdl. Note that the library gets installed by
# install-data, so that's where we need to hook.
install-data-hook:
	rm -f $(DESTDIR)$(rubylibdir)/PinkTrace.la
endif
