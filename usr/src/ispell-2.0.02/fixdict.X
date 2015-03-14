#!/bin/sh
#
#	Add capitalization information to an ispell dictionary
#
#	Usage:
#
#	fixdict dict-file
#
#	Requires availability of UNIX spell.  The new dictionary is
#	rewritten in place.  A list of words that couldn't be
#	resolved (because spell doesn't know them) is written to
#	standard output.  This list appears in lowercase in the
#	dictionary, and if there are any errors the must be edited
#	by hand.
#
#	The final dictionary appears in expanded form and must be
#	passed through munchlist to regenerate suffixes.
#
LIBDIR=!!LIBDIR!!
EXPAND1=${LIBDIR}/isexp1.sed
EXPAND2=${LIBDIR}/isexp2.sed
EXPAND3=${LIBDIR}/isexp3.sed
EXPAND4=${LIBDIR}/isexp4.sed

# TDIR=${TMPDIR:-/tmp}
TDIR=/tmp
TMP=${TDIR}/fix$$

trap "/bin/rm -f ${TMP}*; exit 1" 1 2 15
sed -f ${EXPAND1} $1 | sed -f ${EXPAND2} \
  | sed -f ${EXPAND3} | sed -f ${EXPAND4} \
  | tr '[A-Z]' '[a-z]' \
  | spell \
  | sort > ${TMP}a
#
# ${TMP}a contains all the words that spell doesn't like.
# Now figure out which of those are because spell doesn't know them at
# all, and leave those in ${TMP}b.
#
tr '[a-z]' '[A-Z]' < ${TMP}a | spell | tr '[A-Z]' '[a-z]' > ${TMP}b
#
# The wrongly-capitalized words are those that spell didn't object to
# in the last step.  Produce a list of them in, and capitalize the
# first letter of each.  Save this list in ${TMP}c.
#
comm -23 ${TMP}a ${TMP}b \
  | sed 's/^a/A/;s/^b/B/;s/^c/C/;s/^d/D/;s/^e/E/;s/^f/F/;s/^g/G/;s/^h/H/
     s/^i/I/;s/^j/J/;s/^k/K/;s/^l/L/;s/^m/M/;s/^n/N/;s/^o/O/;s/^p/P/
     s/^q/Q/;s/^r/R/;s/^s/S/;s/^t/T/;s/^u/U/;s/^v/V/;s/^w/W/;s/^x/X/
     s/^y/Y/;s/^z/Z/' > ${TMP}c
#
# Find out which of those spell objects to, saving the failures in ${TMP}d.
#
spell ${TMP}c > ${TMP}d
#
# Extract the words which were correctly capitalized at the first letter,
# combine them with an all-capitals version of the ones that weren't, and
# put the result into ${TMP}e.
#
(comm -23 ${TMP}c ${TMP}d;  tr '[a-z]' '[A-Z]' < ${TMP}d) \
  | sort -o ${TMP}e
#
# At this point, ${TMP}b contains the words that spell just plain doesn't
# like, and ${TMP}e contains the words that are now capitalized correctly.
#
/bin/rm ${TMP}[cd]
#
# Put it all together, rewriting the dictionary in place.
#
sed -f ${EXPAND1} $1 | sed -f ${EXPAND2} \
  | sed -f ${EXPAND3} | sed -f ${EXPAND4} \
  | tr '[A-Z]' '[a-z]' \
  | sort \
  | comm -23 - ${TMP}a \
  | sort -f -o $1 - ${TMP}b ${TMP}e
#
# Finally, write the list of words that have questionable capitalization
# to the standard output.
#
cat ${TMP}b
/bin/rm ${TMP}*
