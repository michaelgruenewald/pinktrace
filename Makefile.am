CLEANFILES= *~
MAINTAINERCLEANFILES= Makefile.in configure aclocal.m4 \
		      config.h config.h.in INSTALL
ACLOCAL_AMFLAGS= -I m4
AUTOMAKE_OPTIONS= dist-bzip2 no-dist-gzip std-options foreign
EXTRA_DIST= autogen.sh README.mkd misc/generated-file.txt
SUBDIRS= src doc . tests

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=pinktrace.pc

pinktrace_includedir=$(includedir)/pinktrace-$(PINKTRACE_PC_SLOT)/pinktrace/
pinktrace_include_HEADERS= \
			  include/pinktrace/gcc.h \
			  include/pinktrace/bitness.h \
			  include/pinktrace/decode.h \
			  include/pinktrace/encode.h \
			  include/pinktrace/error.h \
			  include/pinktrace/event.h \
			  include/pinktrace/fork.h \
			  include/pinktrace/socket.h \
			  include/pinktrace/trace.h \
			  include/pinktrace/util.h \
			  include/pinktrace/pink.h

noinst_HEADERS= include/pinktrace/internal.h

doxygen:
	$(MAKE) -C doc $@
