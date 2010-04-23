EXTRA_DIST= doxygen.conf.in
MAINTAINERCLEANFILES = Makefile.in doxygen.conf
CLEANFILES= *~ reference/* libpinktrace.tag

if HAVE_DOXYGEN
doxygen: doxygen.conf
	doxygen doxygen.conf
else
doxygen:
	@echo "You don't have doxygen installed!"
	exit 1
endif

upload: doxygen
	rsync -avze ssh --delete reference/html bach.exherbo.org:public_html/pinktrace/reference

.PHONY: doxygen upload
