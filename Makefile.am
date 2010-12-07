CLEANFILES= *~ pinktrace.cabal
MAINTAINERCLEANFILES= Makefile.in configure aclocal.m4 \
		      config.h config.h.in INSTALL
ACLOCAL_AMFLAGS= -I m4
AUTOMAKE_OPTIONS= dist-bzip2 no-dist-gzip std-options foreign
EXTRA_DIST= autogen.sh haddock.sh COPYRIGHT README.mkd TODO.mkd misc/generated-file.txt
SUBDIRS= src tests haskell python ruby doc pkg-config

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
		     include/pinktrace/easy/call.h \
		     include/pinktrace/easy/callback.h \
		     include/pinktrace/easy/context.h \
		     include/pinktrace/easy/error.h \
		     include/pinktrace/easy/exec.h \
		     include/pinktrace/easy/loop.h \
		     include/pinktrace/easy/process.h \
		     include/pinktrace/easy/pink.h
EXTRA_DIST+= $(pinktrace_easy_DIST) include/pinktrace/easy/internal.h
if WANT_EASY
pinktrace_easy_includedir=$(includedir)/pinktrace-$(PINKTRACE_PC_SLOT)/pinktrace/easy
pinktrace_easy_include_HEADERS= $(pinktrace_easy_DIST)
endif # WANT_EASY

EXTRA_DIST+= include/pinktrace/about.h.in include/pinktrace/bitness.h.in
noinst_HEADERS= include/pinktrace/internal.h

noinst_DATA= pinktrace.cabal Setup.lhs
pinktrace.cabal: pinktrace.cabal.in Makefile
	$(AM_V_GEN)
	$(AM_V_at)$(SED) \
		-e "s|#VERSION#|$(VERSION)|" \
		-e "s|#PINKTRACE_CFLAGS#|$(PINKTRACE_CFLAGS)|" \
		-e "s|#PINKTRACE_PC_SLOT#|$(PINKTRACE_PC_SLOT)|" \
		-e "s|#TOP_BUILDDIR#|$(top_builddir)|" \
		-e "s|#TOP_SRCDIR#|$(top_srcdir)|" \
		< $(srcdir)/pinktrace.cabal.in > pinktrace.cabal
Setup.lhs: Setup.lhs.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) \
		-e "s|#TOP_BUILDDIR#|$(top_builddir)|" \
		< $(srcdir)/Setup.lhs.in > Setup.lhs
	$(AM_V_at)chmod +x Setup.lhs

checksum: dist
	@echo "SHA1 $(PACKAGE)-$(VERSION).tar.bz2"
	sha1sum $(PACKAGE)-$(VERSION).tar.bz2 > $(PACKAGE)-$(VERSION).tar.bz2.sha1sum
	@echo "SIGN $(PACKAGE)-$(VERSION).tar.bz2"
	gpg --detach-sign --armor $(PACKAGE)-$(VERSION).tar.bz2

upload-release:
	@echo "UPLOAD $(PACKAGE)-$(VERSION).tar.bz2*"
	scp $(PACKAGE)-$(VERSION).tar.bz2* bach.exherbo.org:public_html/pinktrace/release

doxygen:
	$(MAKE) -C doc $@

epydoc:
	$(MAKE) -C doc $@

rdoc:
	$(MAKE) -C doc $@

upload-doc:
	$(MAKE) -C doc $@

upload-example:
	$(MAKE) -C examples $@

upload-www:
	$(MAKE) -C doc $@

upload: upload-doc upload-example upload-www

.PHONY: doxygen epydoc rdoc upload upload-doc upload-example upload-www upload-release
