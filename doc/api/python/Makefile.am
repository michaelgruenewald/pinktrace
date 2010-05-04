SUBDIRS= .
EXTRA_DIST= epydoc.css
EPYDOC_FLAGS= --verbose \
	      --name "pinktrace" \
	      --url "http://dev.exherbo.org/~alip/pinktrace/api/python" \
	      --no-frames \
	      --css $(srcdir)/epydoc.css
REQUIRED_SYMLINKS= $(top_builddir)/python/pinktrace/bitness.so \
		   $(top_builddir)/python/pinktrace/event.so \
		   $(top_builddir)/python/pinktrace/fork.so \
		   $(top_builddir)/python/pinktrace/socket.so \
		   $(top_builddir)/python/pinktrace/string.so \
		   $(top_builddir)/python/pinktrace/syscall.so \
		   $(top_builddir)/python/pinktrace/trace.so

if ENABLE_PYTHON_DOC
all-local: epydoc

epydoc: python

$(top_builddir)/python/pinktrace/%.so: $(top_builddir)/python/pinktrace/.libs/%.so
	test -h $@ || $(LN_S) $< $@

python: $(REQUIRED_SYMLINKS)
	$(AM_V_GEN)
	$(AM_V_at)rm -fr python
	$(AM_V_at)mkdir -p python
	PYTHON_PATH=$(top_builddir)/python $(EPYDOC) $(EPYDOC_FLAGS) -o $@ \
		    $(top_builddir)/python/pinktrace

install-data-local:
	mkdir -p $(DESTDIR)$(htmldir)/api
	cp -R python $(DESTDIR)$(htmldir)/api

upload-doc: epydoc
	rsync -avze ssh --delete python bach.exherbo.org:public_html/pinktrace/api

.PHONY: epydoc symlinks upload-doc
else
upload-doc:
	@echo "Not uploading Python API documentation"
	exit 0

.PHONY: upload-doc
endif

clean-local:
	rm -fr python

uninstall-local:
	rm -fr $(DESTDIR)$(htmldir)/api/python
