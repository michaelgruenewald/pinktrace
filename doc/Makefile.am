EXTRA_DIST= doxygen.conf.in
MAINTAINERCLEANFILES = Makefile.in doxygen.conf
CLEANFILES= *~ api/c/* libpinktrace.tag

if HAVE_DOXYGEN
all-local: doxygen

doxygen: api

api: doxygen.conf
	doxygen doxygen.conf

install-data-local:
	mkdir -p $(DESTDIR)/$(htmldir)/api
	cp -R api/c $(DESTDIR)/$(htmldir)/api

else
doxygen:
	@echo "You don't have doxygen installed!"
	exit 1
endif

upload: doxygen
	rsync -avze ssh --delete api/c bach.exherbo.org:public_html/pinktrace/api

.PHONY: doxygen upload
