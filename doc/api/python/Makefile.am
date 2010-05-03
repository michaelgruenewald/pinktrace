SUBDIRS= .
EXTRA_DIST= epydoc.css
EPYDOC_FLAGS= --verbose \
	      --name "pinktrace" \
	      --url "http://dev.exherbo.org/~alip/pinktrace/api/python" \
	      --no-frames \
	      --css $(srcdir)/epydoc.css

if ENABLE_PYTHON_DOC
all-local: epydoc

epydoc: python

symlinks:
	test -e $(top_builddir)/python/pinktrace/about.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/about.so about.so )
	test -e $(top_builddir)/python/pinktrace/bitness.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/bitness.so bitness.so )
	test -e $(top_builddir)/python/pinktrace/event.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/event.so event.so )
	test -e $(top_builddir)/python/pinktrace/fork.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/fork.so fork.so )
	test -e $(top_builddir)/python/pinktrace/socket.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/socket.so socket.so )
	test -e $(top_builddir)/python/pinktrace/string.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/string.so string.so )
	test -e $(top_builddir)/python/pinktrace/syscall.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/syscall.so syscall.so )
	test -e $(top_builddir)/python/pinktrace/trace.so || \
		( cd $(top_builddir)/python/pinktrace && $(LN_S) .libs/trace.so trace.so )

python: symlinks
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
