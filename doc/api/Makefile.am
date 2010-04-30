SUBDIRS= c ruby .

doxygen:
	$(MAKE) -C c $@

rdoc:
	$(MAKE) -C ruby $@

upload-doc:
	$(MAKE) -C c $@
	$(MAKE) -C ruby $@

.PHONY: doxygen rdoc upload-doc
