SUBDIRS= .
EPYDOC_FLAGS= --name "pinktrace" \
	      --url "http://dev.exherbo.org/~alip/pinktrace/api/python"

if ENABLE_PYTHON_DOC
all-local: epydoc

epydoc: python

python: $(top_builddir)/python/.libs/pinktrace.so
	$(AM_V_GEN)
	$(AM_V_at)rm -fr python
	$(AM_V_at)mkdir -p python
	PYTHON_PATH=$(top_builddir)/python/.libs $(EPYDOC) $(EPYDOC_FLAGS) -o $@ $<

install-data-local:
	mkdir -p $(DESTDIR)$(htmldir)/api
	cp -R python $(DESTDIR)$(htmldir)/api

upload-doc: epydoc
	rsync -avze ssh --delete python bach.exherbo.org:public_html/pinktrace/api

.PHONY: epydoc upload-doc
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
