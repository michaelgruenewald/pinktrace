SUBDIRS= c python ruby .
RSYNC= rsync -avze ssh --delete \
       --filter '+ c/' \
       --filter '+ python/' \
       --filter '+ ruby/' \
       --filter '+ c/*.c' \
       --filter '+ python/*.py' \
       --filter '+ ruby/*.rb' \
       --filter '- *'

upload-example:
	$(RSYNC) . bach.exherbo.org:public_html/examples
