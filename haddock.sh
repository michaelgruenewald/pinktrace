#!/bin/sh
# vim: set sw=4 et sts=4 tw=80 :

# Generate Haskell API documentation and upload
HOST="bach.exherbo.org"
DESTINATION="public_html/pinktrace/api/haskell"
HACKAGE='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'
OUTPUT_DIRECTORY="dist/doc/html/pinktrace"

cabal haddock --html-location="${HACKAGE}" || exit 1

rsync -avze ssh \
    --delete \
    --partial \
    --progress \
    ${OUTPUT_DIRECTORY}/* "${HOST}":"${DESTINATION}"/ || exit 2
