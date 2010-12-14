SUBDIRS= .
MAINTAINERCLEANFILES = \
		       Makefile.in \
		       doxygen.conf
CLEANFILES= *~ c/* libpinktrace.tag

doxygen_files= \
	       groups.doxygen \
	       main_page.doxygen
EXTRA_DIST= doxygen.conf.in $(doxygen_files)

if HAVE_DOXYGEN
all-local: doxygen

doxygen: c

c: doxygen.conf $(doxygen_files)
	$(AM_V_GEN)
	$(AM_V_at)doxygen doxygen.conf

install-data-local:
	mkdir -p $(DESTDIR)$(htmldir)/api
	cp -R c $(DESTDIR)$(htmldir)/api

upload-doc: doxygen
	rsync -avze ssh --delete --partial --progress c bach.exherbo.org:public_html/pinktrace/api

else
doxygen:
	@echo "You don't have doxygen installed!"
	exit 1
upload-doc:
	@echo "Not uploading C API documentation"
	exit 0
endif

clean-local:
	rm -fr c

unistall-data-local:
	rm -fr $(DESTDIR)$(htmldir)/api/c

.PHONY: doxygen upload-doc
