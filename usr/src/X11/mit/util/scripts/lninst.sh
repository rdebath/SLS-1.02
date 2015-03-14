#!/bin/sh

#
# This accepts bsd-style install arguments and simply makes symbolic links.
#

flags=""
dst=""
src=""
dostrip=""

while [ x$1 != x ]; do
    case $1 in 
	-c) shift
	    continue;;

	-[mog]) flags="$flags $1 $2 "
	    shift
	    shift
	    continue;;

	-s) dostrip="strip"
	    shift
	    continue;;

	*)  if [ x$src = x ] 
	    then
		src=$1
	    else
		dst=$1
	    fi
	    shift
	    continue;;
    esac
done

if [ x$src = x ] 
then
	echo "syminst:  no input file specified"
	exit 1
fi

if [ x$dst = x ] 
then
	echo "syminst:  no destination specified"
	exit 1
fi

if [ -d $dst ]; then
    rm -f $dst/`basename $src`
else
    rm -f $dst
fi

ln -s `pwd`/$src $dst
