IF_RUBY_FILES= pink-ruby.c
IF_RUBY_TESTS= \
	       ruby-test.sh \
	       TEST_trace.rb \
	       TEST_bitness.rb \
	       TEST_syscall_freebsd.rb \
	       TEST_string_freebsd.rb \
	       TEST_strarray_freebsd.rb \
	       TEST_socket_address_freebsd.rb \
	       TEST_event.rb \
	       TEST_syscall_linux.rb \
	       TEST_string_linux.rb \
	       TEST_strarray_linux.rb \
	       TEST_socket_linux.rb \
	       TEST_socket_address_linux.rb
EXTRA_DIST= $(IF_RUBY_FILES) $(IF_RUBY_TESTS)
CLEANFILES= TEST_UNIX_SOCKET

if ENABLE_RUBY
AM_CFLAGS= \
	   -I. -I@RUBY_HDRDIR@ -I@RUBY_HDRARCHDIR@ \
	   -I$(top_srcdir)/include \
	   @PINKTRACE_CFLAGS@ @PINKTRACE_RUBY_CFLAGS@

rubylibdir= @RUBY_INSTALL_DIR@
rubylib_LTLIBRARIES= PinkTrace.la

PinkTrace_la_SOURCES= $(IF_RUBY_FILES)
PinkTrace_la_LDFLAGS= -module -avoid-version -shared
PinkTrace_la_LIBADD= $(top_builddir)/src/libpinktrace_@PINKTRACE_PC_SLOT@.la

TESTS_ENVIRONMENT= RUBY="$(RUBY)" ./ruby-test.sh
TESTS= \
       TEST_trace.rb \
       TEST_bitness.rb

if FREEBSD
TESTS+= \
	TEST_syscall_freebsd.rb \
	TEST_string_freebsd.rb \
	TEST_strarray_freebsd.rb \
	TEST_socket_address_freebsd.rb
endif # FREEBSD

if LINUX
TESTS+= \
	TEST_event.rb \
	TEST_syscall_linux.rb \
	TEST_string_linux.rb \
	TEST_strarray_linux.rb \
	TEST_socket_linux.rb \
	TEST_socket_address_linux.rb
endif # LINUX

# Remove the .la file - PinkTrace.la is never linked against (it's a module)
# and Ruby doesn't use libltdl. Note that the library gets installed by
# install-data, so that's where we need to hook.
install-data-hook:
	rm -f $(DESTDIR)$(rubylibdir)/PinkTrace.la
endif # ENABLE_RUBY
