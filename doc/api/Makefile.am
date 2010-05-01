SUBDIRS= c python ruby .

doxygen:
	$(MAKE) -C c $@

epydoc:
	$(MAKE) -C python $@

rdoc:
	$(MAKE) -C ruby $@

upload-doc:
	$(MAKE) -C c $@
	$(MAKE) -C ruby $@
	$(MAKE) -C python $@

.PHONY: doxygen epydoc rdoc upload-doc
