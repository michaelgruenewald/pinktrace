CLEANFILES= *~ pinktrace.cabal
MAINTAINERCLEANFILES= Makefile.in configure aclocal.m4 \
		      config.h config.h.in INSTALL
ACLOCAL_AMFLAGS= -I m4
AUTOMAKE_OPTIONS= dist-bzip2 no-dist-gzip std-options foreign
EXTRA_DIST= autogen.sh COPYRIGHT README.mkd misc/generated-file.txt
SUBDIRS= . src tests python ruby doc pkg-config

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

noinst_HEADERS= include/pinktrace/internal.h

noinst_DATA= pinktrace.cabal
pinktrace.cabal: pinktrace.cabal.in Makefile
	$(AM_V_GEN)
	$(AM_V_at)$(SED) \
		-e "s|#VERSION#|$(VERSION)|" \
		-e "s|#PINKTRACE_CFLAGS#|$(PINKTRACE_CFLAGS)|" \
		-e "s|#PINKTRACE_PC_SLOT#|$(PINKTRACE_PC_SLOT)|" \
		-e "s|#TOP_BUILDDIR#|$(top_builddir)|" \
		-e "s|#TOP_SRCDIR#|$(top_srcdir)|" \
		< $(srcdir)/pinktrace.cabal.in > pinktrace.cabal

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

.PHONY: doxygen epydoc rdoc upload upload-doc upload-example upload-www
