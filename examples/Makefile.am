SUBDIRS= c haskell python ruby .
RSYNC= rsync -avze ssh --delete \
       --filter '+ c/' \
       --filter '+ haskell/' \
       --filter '+ python/' \
       --filter '+ ruby/' \
       --filter '+ haskell/*.hs' \
       --filter '+ c/*.c' \
       --filter '+ python/*.py' \
       --filter '+ ruby/*.rb' \
       --filter '- *'

upload-example:
	$(RSYNC) . bach.exherbo.org:public_html/pinktrace/examples

.PHONY: upload-example
