#!/bin/sh

case "$1" in
"")	echo "Usage: $0 directory"; exit 1;;
esac

echo "Analyzing $1 for Incompatabilities with System V"

echo 'File names longer than 12 characters (excluding the doc directory):'
cd $1

dirlist=
for dir in `echo *`
do
	case "$dir" in
	doc)	;;
	*)	dirlist="$dirlist $dir";;
	esac
done

(
	find doc      -name '???????????????*' -print
	find $dirlist -name '?????????????*' -print
) | sort \
  | sed -e '/,v/d' \
		-e 's/^/	/'

echo 'Symbolic links:'
find . -type l -print | sed -e 's/^/	/'

