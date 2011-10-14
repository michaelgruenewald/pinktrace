SUBDIRS= api .
GENERATED_FILES= \
		 c-pink-about.html \
		 c-pink-fork-freebsd.html \
		 c-pink-fork-linux.html \
		 c-pink-simple-strace-freebsd.html \
		 c-pink-simple-strace-linux.html \
		 hs-pink-about.html \
		 hs-pink-fork-freebsd.html \
		 hs-pink-fork-linux.html \
		 py-pink-about.html \
		 py-pink-fork-freebsd.html \
		 py-pink-fork-linux.html \
		 py-pink-simple-strace-freebsd.html \
		 py-pink-simple-strace-linux.html \
		 rb-pink-about.html \
		 rb-pink-fork-freebsd.html \
		 rb-pink-fork-linux.html \
		 rb-pink-simple-strace-freebsd.html \
		 rb-pink-simple-strace-linux.html
CLEANFILES= $(GENERATED_FILES)
WWW_FILES= \
	   _layouts/default.html \
	   _includes/footer.html \
	   _includes/menu.html \
	   _scripts/examples.rb \
	   css/syntax.css \
	   css/screen.css \
	   images/header.png \
	   images/favicon.png
EXTRA_DIST= $(WWW_FILES)

if ENABLE_SITE
c-%.html: $(top_srcdir)/examples/c/%.c
	$(AM_V_GEN)
	$(AM_V_at)_scripts/examples.rb $< > $@

hs-%.html: $(top_srcdir)/examples/haskell/%.hs
	$(AM_V_GEN)
	$(AM_V_at)_scripts/examples.rb $< > $@

py-%.html: $(top_srcdir)/examples/python/%.py
	$(AM_V_GEN)
	$(AM_V_at)_scripts/examples.rb $< > $@

rb-%.html: $(top_srcdir)/examples/ruby/%.rb
	$(AM_V_GEN)
	$(AM_V_at)_scripts/examples.rb $< > $@
endif # ENABLE_SITE

if ENABLE_SITE_COPY
site: $(GENERATED_FILES) $(WWW_FILES)
	$(RSYNC) -av --filter '- Makefile*' --filter '- *.in' --filter '- api' --filter '- _scripts' --filter '- _site' . $(SITE_INSTALL_DIR)/
	$(MAKE) -C api $@
site-check:
	@echo Run jekyll in $(SITE_INSTALL_DIR)
	exit 0
endif

if ENABLE_SITE_GEN
site: $(GENERATED_FILES) $(WWW_FILES)
	$(JEKYLL) --pygments $(SITE_INSTALL_DIR)

site-check: $(GENERATED_FILES) $(WWW_FILES)
	$(JEKYLL) --pygments --server $(SITE_INSTALL_DIR)
endif

doxygen:
	$(MAKE) -C api $@

epydoc:
	$(MAKE) -C api $@

rdoc:
	$(MAKE) -C api $@

.PHONY: site site-check doxygen epydoc rdoc
