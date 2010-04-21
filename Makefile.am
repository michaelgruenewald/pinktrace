CLEANFILES= *~
MAINTAINERCLEANFILES= Makefile.in configure aclocal.m4 \
		      config.h config.h.in INSTALL
ACLOCAL_AMFLAGS= -I m4
AUTOMAKE_OPTIONS= dist-bzip2 no-dist-gzip std-options foreign
EXTRA_DIST= autogen.sh README.mkd
SUBDIRS= src doc . tests

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=pinktrace.pc

pinktraceincludedir= $(includedir)/pinktrace
pinktraceinclude_HEADERS= \
			  include/pinktrace/gcc.h \
			  include/pinktrace/bitness.h \
			  include/pinktrace/error.h \
			  include/pinktrace/event.h \
			  include/pinktrace/step.h \
			  include/pinktrace/context.h \
			  include/pinktrace/fork.h \
			  include/pinktrace/loop.h \
			  include/pinktrace/trace.h \
			  include/pinktrace/util.h \
			  include/pinktrace/pink.h

noinst_HEADERS= include/pinktrace/internal.h

doxygen:
	$(MAKE) -C doc $@
