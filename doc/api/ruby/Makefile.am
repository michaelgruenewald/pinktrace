SUBDIRS= .
RDOC_FLAGS= -t "PinkTrace Ruby API" -m "PinkTrace"

if ENABLE_RUBY_DOC
all-local: rdoc

rdoc: ruby

ruby: $(top_srcdir)/ruby/pink-ruby.c
	$(AM_V_GEN)
	$(AM_V_at)rm -fr ruby
	$(AM_V_at)$(RDOC) $(RDOC_FLAGS) --op $@ $<

install-data-local:
	mkdir -p $(DESTDIR)$(htmldir)/api
	cp -R ruby $(DESTDIR)$(htmldir)/api

if ENABLE_SITE
site:
	mkdir -p $(SITE_INSTALL_DIR)/api
	cp -R ruby $(SITE_INSTALL_DIR)/api
endif
else
site:
	@echo "Can't copy Ruby API docs"
	exit 0
endif

clean-local:
	rm -fr ruby

uninstall-local:
	rm -fr $(DESTDIR)$(htmldir)/api/ruby

.PHONY: rdoc site
