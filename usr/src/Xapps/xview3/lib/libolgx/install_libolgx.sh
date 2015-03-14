#! /bin/sh -

#
# The first argument is ${INSTALL}, the second is ${DESTDIR}.
#
INSTALL=$1
VERS_FILE=$4
DESTDIR=$5

#
# The black magic in VERSION_S5 adds one to the minor version number in
# "version" and leaves the minor version number alone.
# Stop auto incrementing the minor version number - Janice Hsia 6/30/88

version_42=`cat $VERS_FILE`

version_s5=$version_42

$INSTALL libolgx.so $DESTDIR/usr/lib/libolgx.so.$version_s5
