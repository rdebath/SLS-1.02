#!/bin/sh
#
#	Make a beginning dictionary file for ispell, using an existing
#	speller.
#
#	Usage:
#
#	makedict file-list
#
#	The specified files are collected, split into words, and run through
#	the system speller (usually spell(1)).  Any words that the speller
#	accepts will be written to the standard output for use in making
#	an ispell dictionary.  Usually, you will want to run the output
#	of this script through "munchlist" to get a final dictionary.
#

# This program must produce a list of INCORRECTLY spelled words on standard
# output, given a list of words on standard input.  If you don't have a
# speller, but do have a lot of correctly-spelled files, try /bin/true.
#

# SPELLPROG="${SPELLPROG:-spell}"
SPELLPROG=/usr/bin/spell

# TMP=${TMPDIR:-/tmp}/mkdict$$
TMP=/tmp/mkdict$$

case "$#" in
    0)
	set X -
	shift
	;;
esac

trap "/bin/rm ${TMP}*; exit 1" 1 2 15

# cat "$@" | tr -cs "A-Za-z'" '\012' | sort -uf -o ${TMP}
# $SPELLPROG < ${TMP} | comm -13 - ${TMP}
# /bin/rm ${TMP}

# Use less processes at the expense of more temporary files.

cat "$@" | tr -cs "A-Za-z'" '\012' > ${TMP}a
sort -uf -o ${TMP}b < ${TMP}a
$SPELLPROG < ${TMP}b > ${TMP}c
comm -13 ${TMP}c ${TMP}b
/bin/rm ${TMP}*

