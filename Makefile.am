CLEANFILES= *~
MAINTAINERCLEANFILES= Makefile.in configure aclocal.m4 \
		      config.h config.h.in INSTALL
ACLOCAL_AMFLAGS= -I m4
AUTOMAKE_OPTIONS= dist-bzip2 no-dist-gzip std-options foreign
EXTRA_DIST= autogen.sh COPYRIGHT README.mkd NEWS.mkd TODO.mkd misc/generated-file.txt
SUBDIRS= . src tests python ruby doc examples pkg-config

pinktrace_includedir=$(includedir)/pinktrace-$(PINKTRACE_PC_SLOT)/pinktrace/
pinktrace_include_HEADERS= \
			  include/pinktrace/about.h \
			  include/pinktrace/bitness.h \
			  include/pinktrace/decode.h \
			  include/pinktrace/encode.h \
			  include/pinktrace/event.h \
			  include/pinktrace/gcc.h \
			  include/pinktrace/name.h \
			  include/pinktrace/socket.h \
			  include/pinktrace/trace.h \
			  include/pinktrace/util.h \
			  include/pinktrace/pink.h

EXTRA_DIST+= include/pinktrace/about.h.in include/pinktrace/bitness.h.in
noinst_HEADERS= include/pinktrace/internal.h

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
