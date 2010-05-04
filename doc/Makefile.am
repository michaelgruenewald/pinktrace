SUBDIRS= api .
HOMEPAGE= http://dev.exherbo.org/~alip/pinktrace
SCM_URI= git://github.com/alip/pinktrace.git
GENERATED_FILES= _config.yml \
		 c-pink-about.mkd \
		 c-pink-fork.mkd \
		 c-pink-simple-strace.mkd \
		 py-pink-about.mkd \
		 py-pink-fork.mkd \
		 py-pink-simple-strace.mkd \
		 rb-pink-about.mkd \
		 rb-pink-fork.mkd \
		 rb-pink-simple-strace.mkd
CLEANFILES= $(GENERATED_FILES)
WWW_FILES= _layouts/default.html \
	   _includes/footer.html \
	   _includes/google-analytics.html \
	   _includes/menu.html \
	   _scripts/examples.rb \
	   css/syntax.css \
	   css/screen.css \
	   images/favicon.png
EXTRA_DIST= $(WWW_FILES)

%.yml: %.yml.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) \
		-e "s,##VERSION##,$(VERSION),g" \
		-e "s,##HOMEPAGE##,$(HOMEPAGE),g" \
		-e "s,##SCM_URI##,$(SCM_URI),g" \
		$< > $@

c-%.mkd: ../examples/c/%.c
	$(AM_V_GEN)
	$(AM_V_at)_scripts/examples.rb $< > $@

py-%.mkd: ../examples/python/%.py
	$(AM_V_GEN)
	$(AM_V_at)_scripts/examples.rb $< > $@

rb-%.mkd: ../examples/ruby/%.rb
	$(AM_V_GEN)
	$(AM_V_at)_scripts/examples.rb $< > $@

clean-local:
	rm -fr _site

site: $(GENERATED_FILES) $(WWW_FILES)
	jekyll --pygments

site-check:
	jekyll --pygments --server

doxygen:
	$(MAKE) -C api $@

epydoc:
	$(MAKE) -C api $@

rdoc:
	$(MAKE) -C api $@

upload-doc:
	$(MAKE) -C api $@

upload-www: site
	rsync -avze ssh \
		--delete --filter '- api' \
		_site/* bach.exherbo.org:public_html/pinktrace/

.PHONY: site site-check doxygen epydoc rdoc upload-doc
