#!/bin/sh
# autoheader -- create `config.h.in' from `configure.in'
# Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

# Written by Roland McGrath.

# If given no args, create `config.h.in' from template file `configure.in'.
# With one arg, create a header file on standard output from
# the given template file.

usage="Usage: autoheader [-v] [--version] [template-file]"

MACRODIR=@datadir@
TEMPLATES=$MACRODIR/acconfig.h
test -r acconfig.h && TEMPLATES="$TEMPLATES acconfig.h"
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

# These are the alternate definitions of the acgeneral.m4 macros we want to
# redefine.  They produce strings in the output marked with "@@@" so we can
# easily extract the information we want.
frob='define([AC_DEFINE],[
@@@syms="$syms $1"@@@
])dnl
define([AC_HAVE_FUNCS],[
@@@funcs="$funcs $1"@@@
])dnl
define([AC_HAVE_HEADERS],[
@@@headers="$headers $1"@@@
])dnl'

# We extract assignments of SYMS, FUNCS, and HEADERS from the modified
# autoconf processing of the input file.
eval "`echo \"$frob\" \
       | m4 $MACROFILES - $infile \
       | sed -n '/^@@@/,/@@@$/s/^@*\([^@]*\)@*$/\1/p'`"

# Make SYMS newline-separated rather than blank-separated.
syms="`for sym in $syms; do echo $sym; done`"

test $# -eq 0 && exec > config.h.in

echo "/* Generated automatically from $infile by autoheader.  DO NOT EDIT!  */"
echo ''

# Turn newlines into @s, then double-@s back into newlines.
# This puts each paragraph on its own line, separated by @s.
if test -n "$syms"; then
  # Make sure blank lines are really blank so that @@ below wins.
  sed 's/^[ 	]*$//' $TEMPLATES | tr \\012 @ | 
  sed -e 's/@@/
/g' |
  # Select each paragraph that refers to a symbol we picked out above.
  fgrep "$syms" |
  sed '$!s/$/@/' | tr @ \\012
fi

for func in $funcs; do
  sym="`echo ${func} | sed 's/[^a-zA-Z0-9_]/_/g' | tr a-z A-Z`"
  echo "
/* Define if you have ${func}.  */
#undef HAVE_${sym}"
done

for header in $headers; do
  sym="`echo ${header} | sed 's/[^a-zA-Z0-9_]/_/g' | tr a-z A-Z`"
  echo "
/* Define if you have the <${header}> header file.  */
#undef HAVE_${sym}"
done

status=0

for sym in $syms; do
  if fgrep $sym $TEMPLATES >/dev/null; then
    : # All is well.
  else
    echo "$0: Symbol \`${sym}' is not covered by $TEMPLATES" >&2
    status=1
  fi
done

exit $status
