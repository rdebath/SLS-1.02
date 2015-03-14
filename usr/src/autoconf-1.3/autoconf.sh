#!/bin/sh
# autoconf -- create `configure' using m4 macros
# Copyright (C) 1992 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# If given no args, create `configure' from template file `configure.in'.
# With one arg, create a configure script on standard output from
# the given template file.

usage="Usage: autoconf [-v] [--version] [template-file]"

MACRODIR=@datadir@
MACROFILES="$MACRODIR/acgeneral.m4 $MACRODIR/acspecific.m4"
test -r $MACRODIR/aclocal.m4 &&
  MACROFILES="$MACROFILES $MACRODIR/aclocal.m4"
test -r aclocal.m4 && MACROFILES="$MACROFILES aclocal.m4"

if test $# -gt 0; then
  case "$1" in
    -v | --version | --versio | --versi | --vers | --ver | --ve | --v)
    MACROFILES="-DAC_PRINT_VERSION $MACROFILES"; shift ;;
    -*) echo "$usage" >&2; exit 1 ;;
  esac
fi

case $# in
  0) infile=configure.in ;;
  1) infile=$1 ;;
  *) echo "$usage" >&2; exit 1 ;;
esac

if test ! -s $infile; then
  echo "autoconf: ${infile}: No such file or directory" >&2
  exit 1
fi

tmpout=/tmp/acout.$$
trap 'rm -f $tmpout; exit 1' 1 3 15

m4 $MACROFILES $infile > $tmpout

# You could add your own prefixes to pattern if you wanted to check for
# them too, e.g. pattern="AC_\|ILT_", except that UNIX sed doesn't do
# alternation, and GNU sed is dreadfully slow.  Sigh.
pattern="AC_"

status=0
if grep "${pattern}" $tmpout > /dev/null 2>&1; then
  echo "autoconf: Undefined macros:" >&2
  grep "${pattern}" $tmpout | sed "s/.*\(${pattern}[_A-Z0-9]*\).*/\1/" |
    while read name; do
      grep -n $name $infile /dev/null
    done | sort -u >&2
  status=1
fi

case $# in
  0) cat $tmpout > configure; chmod +x configure ;;
  1) cat $tmpout ;;
esac

rm -f $tmpout
exit $status
