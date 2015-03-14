#!/bin/sh
#
#	Given a list of words for ispell, generate a reduced list
#	in which all possible suffixes have been collapsed.  The reduced
#	list will match the same list as the original.
#
#	Usage:
#
#	munchlist [ -d hashfile ] [ -e ] [ -w chars ] [ file ] ...
#
#	Options:
#
#	-d hashfile
#		Remove any words that are covered by 'hashfile'.  The
#		default is the default ispell dictionary.  The words
#		will be removed only if all suffixes are covered by
#		the hash file.  A hashfile of /dev/null should be
#		specified when the main dictionary is being munched.
#	-e	Economical algorithm.  This will use much less temporary
#		disk space, at the expense of time.  Useful with large files
#		(such as complete dictionaries).
#	-w	Passed on to ispell (specify chars that are part of a word)
#
#	The given input files are merged, then processed by 'ispell -c'
#	to generate possible suffix lists;  these are then combined
#	and reduced.  The final result is written to standard output.
#
#	For portability to older systems, I have avoided getopt.
#
#		Geoff Kuenning
#		2/28/87
#
LIBDIR=!!LIBDIR!!
COMBINE=${LIBDIR}/icombine
EXPAND1=${LIBDIR}/isexp1.sed
EXPAND2=${LIBDIR}/isexp2.sed
EXPAND3=${LIBDIR}/isexp3.sed
EXPAND4=${LIBDIR}/isexp4.sed

# TDIR=${TMPDIR:-/usr/tmp}
TDIR=/tmp
TMP=${TDIR}/munch$$

cheap=no
dictopt=
wchars=
while [ $# != 0 ]
do
    case "$1" in
	-d)
	    case "$2" in
		/dev/null)
		    dictopt=NONE
		    ;;
		*)
		    dictopt="-d $2"
		    ;;
	    esac
	    shift
	    ;;
	-e)
	    cheap=yes
	    ;;
	-w)
	    wchars="-w $2"
	    shift
	    ;;
	*)
	    break
    esac
    shift
done
trap "/bin/rm -f ${TMP}*; exit 1" 1 2 15
#
# Collect all the input and expand all the suffix options (four sed's),
# and preserve (sorted) for later joining in ${TMP}a.
#
if [ $# -eq 0 ]
then
    sed -f $EXPAND1 | sed -f $EXPAND2 \
      | sed -f $EXPAND3 | sed -f $EXPAND4 | sort -u > ${TMP}a
else
    sed -f $EXPAND1 "$@" | sed -f $EXPAND2 \
      | sed -f $EXPAND3 | sed -f $EXPAND4 | sort -u > ${TMP}a
fi
#
# Unless an explicitly null dictionary was specified, remove all
# expanded words that are covered by the dictionary.  This produces
# the final list of expanded words that this dictionary must cover.
# Leave the list in ${TMP}b.
#
if [ "X$dictopt" = "XNONE" ]
then
    ln ${TMP}a ${TMP}b
else
    ispell -l $dictopt -p /dev/null < ${TMP}a > ${TMP}b
fi
#
# Munch the input to generate roots and suffixes (ispell -c).  We are
# only interested in words that have at least one suffix (egrep /);  the
# next step will pick up the rest.  Some of the roots are illegal.  We
# use join to restrict the output to those root words that are found
# in the original dictionary.  In cheap mode, we re-sort this for
# icombine's benefit, and then use icombine to scrunch them together.
#
# Note:  one disadvantage of this pipeline is that for a large file,
# the join and icombine may be sitting around for a long time while ispell
# and sorts run.  You can get rid of this by splitting the pipe, at
# the expense of more temp file space.
#
if [ $cheap = yes ]
then
    ispell $wchars -c -d /dev/null -p /dev/null < ${TMP}b \
      | egrep / | sort -u -t/ +0 -1 +1 \
      | join -t/ - ${TMP}a \
      | sort -u -t/ +0f -1 +0 -1 +1 | $COMBINE > ${TMP}c
else
    ispell $wchars -c -d /dev/null -p /dev/null < ${TMP}b \
      | egrep / | sort -u -t/ +0 -1 +1 \
      | join -t/ - ${TMP}a > ${TMP}c
fi
#
# There is now one slight problem:  the suffix flags X, J, and Z
# are simply the addition of an "S" to the suffixes N, G, and R,
# respectively.  This produces redundant entries in the output file;
# for example, ABBREVIATE/N/X and ABBREVIATION/S.  We must get rid
# of the unnecessary duplicates.  The candidates are those words that
# have only an "S" flag (egrep).  We strip off the "S" (sed), and
# generate a list of roots that might have made these words (ispell -c).
# Of these roots, we select those that have the N, G, or R flags,
# replacing each with the plural equivalent X, J, or Z (sed -n).
# Using join once again, we select those that have legal roots
# and put them in ${TMP}d.
#
if [ $cheap = yes ]
then
    egrep '^[^/]*/S$' ${TMP}c | sed 's@/S$@@' \
      | ispell $wchars -c -d /dev/null -p /dev/null \
      | sed -n -e '/\/N/s/N$/X/p' -e '/\/G/s/G$/J/p' -e '/\/R/s/R$/Z/p' \
      | sort -u -t/ +0 -1 +1 \
      | join -t/ - ${TMP}a \
      | sort -u -t/ +0f -1 +0 -1 +1 \
      | $COMBINE > ${TMP}d
else
    egrep '^[^/]*/S$' ${TMP}c | sed 's@/S$@@' \
      | ispell $wchars -c -d /dev/null -p /dev/null \
      | sed -n -e '/\/N/s/N$/X/p' -e '/\/G/s/G$/J/p' -e '/\/R/s/R$/Z/p' \
      | sort -u -t/ +0 -1 +1 \
      | join -t/ - ${TMP}a > ${TMP}d
fi
/bin/rm -f ${TMP}a
#
# Now we have to eliminate the stuff covered by ${TMP}d from ${TMP}c.
# First, we re-expand the suffixes we just made (four sed's), and let
# ispell re-create the /S version (ispell -c).  We select the /S versions
# only (egrep), sort them (sort) for comm, and use comm to delete these
# from ${TMP}c.  The output of comm (i.e., the trimmed version of
# ${TMP}c) is combined with our special-suffixes file ${TMP}d (sort again)
# and reduced in size (icombine) to produce a final list of all words
# that have at least one suffix.
#
sed -f $EXPAND1 ${TMP}d | sed -f $EXPAND2 | sed -f $EXPAND3 | sed -f $EXPAND4 \
  | ispell $wchars -c -d /dev/null -p /dev/null \
  | egrep '\/S$' | sort -u -t/ +0 -1 +1 | comm -13 - ${TMP}c \
  | sort -u -t/ +0f -1 +0 -1 +1 - ${TMP}d \
  | $COMBINE > ${TMP}e
/bin/rm -f ${TMP}[cd]
#
# Now a slick trick.  Use ispell to select those (root) words from the original
# list (${TMP}b) that are not covered by the suffix list (${TMP}e).  Then we
# merge these with the suffix list, sort it, and use icombine to strip out
# unnecessary capitalizations and produce the final output.
#
ispell $wchars -d /dev/null -p ${TMP}e -l < ${TMP}b \
  | sort -t/ +0f -1 +0 -1 +1 - ${TMP}e \
  | $COMBINE
/bin/rm -f ${TMP}*
