#!/bin/sh

set -e
set -x

rm -fr autom4te.cache
rm -f config.cache
test -d build-aux || mkdir build-aux

libtoolize --copy --force --automake
aclocal -I m4
autoheader
autoconf -Wall
automake --add-missing --copy --foreign
