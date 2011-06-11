SUBDIRS= .
EXAMPLES= \
	  pink-about.hs \
	  pink-fork-freebsd.hs \
	  pink-fork-linux.hs
EXTRA_DIST= $(EXAMPLES)

if ENABLE_HASKELL
examplesdir=$(docdir)/examples/haskell
examples_DATA= $(EXAMPLES)
endif # ENABLE_HASKELL

if ENABLE_SITE
site:
	mkdir -p $(SITE_INSTALL_DIR)/examples/haskell
	cp $(EXAMPLES) $(SITE_INSTALL_DIR)/examples/haskell
endif

.PHONY: site
