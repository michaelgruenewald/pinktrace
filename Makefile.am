CLEANFILES= *~ pinktrace.cabal
MAINTAINERCLEANFILES= Makefile.in configure aclocal.m4 \
		      config.h config.h.in INSTALL
ACLOCAL_AMFLAGS= -I m4
AUTOMAKE_OPTIONS= dist-bzip2 no-dist-gzip std-options foreign
EXTRA_DIST= autogen.sh COPYRIGHT misc/generated-file.txt
SUBDIRS= src tests python ruby doc pkg-config examples

DISTCHECK_CONFIGURE_FLAGS= \
			   --enable-ipv6 \
			   --enable-doxygen \
			   --enable-python --enable-python-doc \
			   --enable-ruby --enable-ruby-doc \
			   --with-ruby-install-dir='$${DESTDIR}$${prefix}/ruby-bindings'

doc_DATA= \
	  README.markdown \
	  NEWS.markdown \
	  TODO.markdown
EXTRA_DIST+= $(doc_DATA)

pinktrace_includedir=$(includedir)/pinktrace-$(PINKTRACE_PC_SLOT)/pinktrace/
pinktrace_include_HEADERS= \
			  include/pinktrace/about.h \
			  include/pinktrace/bitness.h \
			  include/pinktrace/compat.h \
			  include/pinktrace/decode.h \
			  include/pinktrace/encode.h \
			  include/pinktrace/event.h \
			  include/pinktrace/gcc.h \
			  include/pinktrace/name.h \
			  include/pinktrace/socket.h \
			  include/pinktrace/trace.h \
			  include/pinktrace/util.h \
			  include/pinktrace/pink.h

pinktrace_easy_DIST= \
		     include/pinktrace/easy/attach.h \
		     include/pinktrace/easy/call.h \
		     include/pinktrace/easy/callback.h \
		     include/pinktrace/easy/context.h \
		     include/pinktrace/easy/error.h \
		     include/pinktrace/easy/exec.h \
		     include/pinktrace/easy/func.h \
		     include/pinktrace/easy/loop.h \
		     include/pinktrace/easy/process.h \
		     include/pinktrace/easy/pink.h
EXTRA_DIST+= \
	     $(pinktrace_easy_DIST) \
	     include/pinktrace/easy/internal.h \
	     include/pinktrace/easy/internal-util.h
if WANT_EASY
pinktrace_easy_includedir=$(includedir)/pinktrace-$(PINKTRACE_PC_SLOT)/pinktrace/easy
pinktrace_easy_include_HEADERS= $(pinktrace_easy_DIST)
endif # WANT_EASY

EXTRA_DIST+= include/pinktrace/about.h.in include/pinktrace/bitness.h.in
noinst_HEADERS= include/pinktrace/internal.h

checksum: dist
	@echo "SHA1 $(PACKAGE)-$(VERSION).tar.bz2"
	sha1sum $(PACKAGE)-$(VERSION).tar.bz2 > $(PACKAGE)-$(VERSION).tar.bz2.sha1sum
	@echo "SIGN $(PACKAGE)-$(VERSION).tar.bz2"
	gpg --detach-sign --armor $(PACKAGE)-$(VERSION).tar.bz2

doxygen: all
	$(MAKE) -C doc $@

epydoc: all
	$(MAKE) -C doc $@

rdoc: all
	$(MAKE) -C doc $@

site: doxygen epydoc rdoc
	$(MAKE) -C doc $@
	$(MAKE) -C examples $@

site-check: site
	$(MAKE) -C doc $@

.PHONY: doxygen epydoc rdoc site site-check
