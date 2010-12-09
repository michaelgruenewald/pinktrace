SUBDIRS= c haskell python ruby .
RSYNC= rsync \
       -avze ssh \
       --delete \
       --partial \
       --progress \
       --filter '+ c/' \
       --filter '+ haskell/' \
       --filter '+ python/' \
       --filter '+ ruby/' \
       --filter '+ c/*.c' \
       --filter '+ haskell/*.hs' \
       --filter '+ python/*.py' \
       --filter '+ ruby/*.rb' \
       --filter '- *'

upload-example:
	$(RSYNC) . bach.exherbo.org:public_html/pinktrace/examples

.PHONY: upload-example
