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
examples_SCRIPTS= $(EXAMPLES)
endif # ENABLE_PYTHON
