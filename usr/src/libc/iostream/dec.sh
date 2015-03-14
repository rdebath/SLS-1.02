#!/bin/sh

if [ x"$1"x = xx ]
then
  echo please give a version number
  exit 1
else
  version=$1
fi

check=0
all=0

case $2 in
-f)
  check=1
  all=1
  ;;
-a)
  all=1
  ;;
-c)
  check=1
  ;;
esac

CC="/net/irl/users/hlu/local/bin/gcc -V $version -b decstation-3100"
CFLAGS="-O2 -fomit-frame-pointer"
#CFLAGS="-O -g"
CFLAGS="-O -gstabs+"

if [ $all = 1 ]
then
  make CC_FOR_TARGET="$CC" GCC_FOR_TARGET="$CC" \
	GXX_FOR_TARGET="$CC" GXX_FOR_BUILD="$CC" \
  	CC_FOR_BUILD="$CC" GCC_FOR_BUILD="$CC" \
  	CC="$CC" GCC="$CC" CXX="$CC" GXX="$CC" \
	CFLAGS="$CFLAGS" GXXFLAGS="$CFLAGS" CXXFLAGS="$CFLAGS" \
	all
fi

if [ $check = 1 ]
then
  make CC_FOR_TARGET="$CC" GCC_FOR_TARGET="$CC" \
	GXX_FOR_TARGET="$CC" GXX_FOR_BUILD="$CC" \
  	CC_FOR_BUILD="$CC" GCC_FOR_BUILD="$CC" \
  	CC="$CC" GCC="$CC" CXX="$CC" GXX="$CC" \
	CFLAGS="$CFLAGS" GXXFLAGS="$CFLAGS" CXXFLAGS="$CFLAGS" \
	check
fi
