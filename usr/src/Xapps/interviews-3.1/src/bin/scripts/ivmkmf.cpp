XCOMM!/bin/sh
XCOMM 
XCOMM generate a Makefile from an Imakefile outside of the source tree
XCOMM 

usage="usage: $0 [-a]"
do_all=

case "$#" in
	0)	;;
	1)	case "$1" in
			-a)	do_all="yes"
				;;
			*)	echo "$usage" 1>&2; exit 1
				;;
		esac
		;;
	*)	echo "$usage" 1>&2; exit 1
		;;
esac

if [ -f Makefile ]; then 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
fi

case "$do_all" in
	yes)	set -x
		imake CONFIGDIRSPEC -DUseInstalled &&
		make Makefiles &&
		make depend
		;;
	*)	set -x
		imake CONFIGDIRSPEC -DUseInstalled
		;;
esac
