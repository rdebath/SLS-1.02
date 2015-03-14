#!/bin/sh
#
# $Header: /home/x_cvs/mit/util/scripts/bsdinst.sh,v 1.4 1992/05/23 06:58:09 dawes Exp $
#
# This accepts bsd-style install arguments and makes the appropriate calls
# to the System V install.
#
# If /usr/ucb/install is available, use it.   (DHD May 1992)
#
# If '-s' is specified, also run 'mcs -d' (dwex)
#

flags=""
dst=""
src=""
dostrip=""
owner=""
group=""
mode=""
bargs=$*

while [ x$1 != x ]; do
    case $1 in 
	-c) shift
	    continue;;

	-m) flags="$flags $1 $2 "
	    mode="$2"
	    shift
	    shift
	    continue;;

	-o) flags="$flags -u $2 "
	    owner="$2"
	    shift
	    shift
	    continue;;

	-g) flags="$flags $1 $2 "
	    group="$2"
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
	echo "bsdinst:  no input file specified"
	exit 1
fi

if [ x$dst = x ] 
then
	echo "bsdinst:  no destination specified"
	exit 1
fi

if [ -x /usr/ucb/install ]
then
	if [ -d "$dst" ]
	then
		dst=$dst/`basename "$src"`
	fi
	case "$group" in
	"")
		bargs="-g other $bargs"
		;;
	esac
	/usr/ucb/install $bargs
	if [ x$dostrip = xstrip -a -x /usr/bin/mcs ]
	then
		/usr/bin/mcs -d $dst
	fi
	exit 0
fi
	
case "$mode" in
"")
	;;
*)
	case "$owner" in
	"")
		flags="$flags -u root"
		;;
	esac
	;;
esac


# set up some variable to be used later

rmcmd=""
srcdir="."

# if the destination isn't a directory we'll need to copy it first

if [ ! -d $dst ]
then
	dstbase=`basename $dst`
	cp $src /tmp/$dstbase
	rmcmd="rm -f /tmp/$dstbase"
	src=$dstbase
	srcdir=/tmp
	dst="`echo $dst | sed 's,^\(.*\)/.*$,\1,'`"
	if [ x$dst = x ]
	then
		dst="."
	fi
fi


# If the src file has a directory, copy it to /tmp to make install happy

srcbase=`basename $src`

if [ "$src" != "$srcbase" -a "$src" != "./$srcbase" ] 
then
	cp $src /tmp/$srcbase
	src=$srcbase
	srcdir=/tmp
	rmcmd="rm -f /tmp/$srcbase"
fi

# do the actual install

if [ -f /usr/sbin/install ]
then
	installcmd=/usr/sbin/install
elif [ -f /etc/install ]
then
	installcmd=/etc/install
else
	installcmd=install
fi

# This rm is commented out because some people want to be able to
# install through symbolic links.  Uncomment it if it offends you.
# rm -f $dst/$srcbase
(cd $srcdir ; $installcmd -f $dst $flags $src)

if [ x$dostrip = xstrip ]
then
	strip $dst/$srcbase
	if [ -x /usr/bin/mcs ]
	then
		/usr/bin/mcs -d $dst/$srcbase
	fi
fi

# and clean up

$rmcmd

