CLEANFILES= *~
MAINTAINERCLEANFILES= Makefile.in configure aclocal.m4 \
		      config.h config.h.in INSTALL
ACLOCAL_AMFLAGS= -I m4
AUTOMAKE_OPTIONS= dist-bzip2 no-dist-gzip std-options foreign
EXTRA_DIST= autogen.sh README.mkd misc/generated-file.txt
SUBDIRS= . src tests python ruby doc pkg-config

pinktrace_includedir=$(includedir)/pinktrace-$(PINKTRACE_PC_SLOT)/pinktrace/
pinktrace_include_HEADERS= \
			  include/pinktrace/about.h \
			  include/pinktrace/bitness.h \
			  include/pinktrace/gcc.h \
			  include/pinktrace/pink.h

pinktrace_backend_includedir= $(pinktrace_backend_includedir)/backend
pinktrace_backend_include_HEADERS= \
				   include/pinktrace/backend/name.h \
				   include/pinktrace/backend/socket.h

if BACKEND_PROC
pinktrace_proc_includedir= $(pinktrace_backend_includedir)/proc/
pinktrace_proc_include_HEADERS= \
				include/pinktrace/backend/proc/fs.h \
				include/pinktrace/backend/proc/util.h \
				include/pinktrace/backend/proc/decode.h
endif # BACKEND_PROC

if BACKEND_PTRACE
pinktrace_ptrace_includedir= $(pinktrace_includedir)/ptrace/
pinktrace_ptrace_include_HEADERS= \
				  include/pinktrace/backend/ptrace/decode.h \
				  include/pinktrace/backend/ptrace/encode.h \
				  include/pinktrace/backend/ptrace/event.h \
				  include/pinktrace/backend/ptrace/trace.h \
				  include/pinktrace/backend/ptrace/util.h
endif # BACKEND_PTRACE

noinst_HEADERS= include/pinktrace/internal.h

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
