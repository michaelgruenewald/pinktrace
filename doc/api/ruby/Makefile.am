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

upload-doc: rdoc
	rsync -avze ssh --delete ruby bach.exherbo.org:public_html/pinktrace/api

.PHONY: rdoc upload-doc
else
upload-doc:
	@echo "Not uploading Ruby API documentation"
	exit 0

.PHONY: upload-doc
endif

clean-local:
	rm -fr ruby

uninstall-local:
	rm -fr $(DESTDIR)$(htmldir)/api/ruby
