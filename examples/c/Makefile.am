SUBDIRS= .

examplesdir=$(docdir)/examples/c
examples_DATA= \
	       makefile-freebsd.txt \
	       makefile-linux.txt \
	       pink-about.c \
	       pink-fork-freebsd.c \
	       pink-fork-linux.c \
	       pink-simple-strace-freebsd.c \
	       pink-simple-strace-linux.c

if ENABLE_SITE
site:
	mkdir -p $(SITE_INSTALL_DIR)/examples/c
	cp $(examples_DATA) $(SITE_INSTALL_DIR)/examples/c
endif

.PHONY: site
