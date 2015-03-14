#!/bin/sh
#
# Preprocess $1 to define the cpu type and output its value.
#

imake -T $1 -f /dev/null -s- |\
    sed -e '/# arch/!d' -e 's/^[ ]*# architecture:  //'

exit 0
