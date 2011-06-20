SUBDIRS= .
EXAMPLES= \
	  pink-about.rb \
	  pink-fork-freebsd.rb \
	  pink-fork-linux.rb \
	  pink-simple-strace-freebsd.rb \
	  pink-simple-strace-linux.rb
EXTRA_DIST= $(EXAMPLES)

if ENABLE_RUBY
examplesdir=$(docdir)/examples/ruby
examples_DATA= $(EXAMPLES)
endif # ENABLE_RUBY

if ENABLE_SITE
site:
	mkdir -p $(SITE_INSTALL_DIR)/examples/ruby
	cp $(EXAMPLES) $(SITE_INSTALL_DIR)/examples/ruby
endif

.PHONY: site
