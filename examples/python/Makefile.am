SUBDIRS= .
EXAMPLES= \
	  pink-about.py \
	  pink-fork-freebsd.py \
	  pink-fork-linux.py \
	  pink-simple-strace-freebsd.py \
	  pink-simple-strace-linux.py
EXTRA_DIST= $(EXAMPLES)

if ENABLE_PYTHON
examplesdir=$(docdir)/examples/python
examples_DATA= $(EXAMPLES)
endif # ENABLE_PYTHON

if ENABLE_SITE
site:
	mkdir -p $(SITE_INSTALL_DIR)/examples/python
	cp $(EXAMPLES) $(SITE_INSTALL_DIR)/examples/python
endif

.PHONY: site
