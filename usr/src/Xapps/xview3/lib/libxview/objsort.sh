#!/bin/sh 
# @(#)objsort.sh 1.1 89/03/01 SMI
#
# Given a preferred ordering of the object modules in the file $1, and a list
# of actual object modules in the directory $2, create a list of
# object modules for the "ld" command line. As time goes on, new modules 
# get created and old ones get deleted without the preferred list getting 
# updated. 
#
# The algorithm here is to delete unused object modules from the preferred
# ordering and then append the new modules to the resulting list.
#
# (based on ECD "objsort")
#

objlist=$1
objdir=$2
myname=`basename $0`

case $# in
2) ;;
*) echo "usage: $myname object-list object-directory" ; exit 1 ;;
esac

OLD=/tmp/$myname.old.$$
NEW=/tmp/$myname.new.$$

trap 'rm -f /tmp/$myname.*.$$; exit 1' 1 2 15

sed '/^#/d;/^$/d' $objlist > $OLD
(cd $objdir && ls *.o) > $NEW
{
	fgrep -f $NEW $OLD
	fgrep -v -f $OLD $NEW
} | sed "s,^,$objdir/,"

rm -f /tmp/$myname.*.$$
