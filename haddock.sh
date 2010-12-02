#!/bin/sh
# vim: set sw=4 et sts=4 tw=80 :

# Generate Haskell API documentation and upload
HOST="bach.exherbo.org"
DESTINATION="public_html/pinktrace/api/haskell"
HACKAGE='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

if [ ! -f ./Setup.lhs ]; then
    echo "./configure" >&2
    exit 1
fi

if [ ! -d ./dist ]; then
    echo "runhaskell Setup.lhs build" >&2
    exit 1
fi

echo ">>> runhaskell Setup.lhs haddock"
runhaskell Setup.lhs haddock \
    "--haddock-options=--html --use-unicode" \
    --html-location="${HACKAGE}" || exit 1

echo ">>> Uploading to ${HOST}:${DESTINATION}"
rsync \
    -avze ssh \
    --delete \
    --partial \
    --progress \
    ./dist/doc/html/pinktrace/ "${HOST}":"${DESTINATION}" || exit 1
