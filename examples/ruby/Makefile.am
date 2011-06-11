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
examples_SCRIPTS= $(EXAMPLES)
endif # ENABLE_RUBY
