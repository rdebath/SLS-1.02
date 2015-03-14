#!/bin/sh

# $Header: /home/x_cvs/mit/server/ddx/x386/configx386.sh,v 1.2 1992/08/30 02:19:04 dawes Exp $
#
# This script generates x386Conf.c
#
# THIS SCRIPT IS NOT USED
#
# usage: configx386.sh driver1 driver2 ...
#

X386CONF=./x386Conf.c
NumScreens=2

cat > $X386CONF <<EOF
/*
 * This file is generated automatically -- DO NOT EDIT
 */

#include "X.h"
#include "Xmd.h"
#include "input.h"
#include "servermd.h"
#include "scrnintstr.h"
#include "site.h"
#include "x386.h"

extern ScrnInfoRec
EOF
Args="$*"
while [ $# -gt 1 ]; do
  echo "        ${1}InfoRec," >> $X386CONF
  shift
done
echo "        ${1}InfoRec;" >> $X386CONF
cat >> $X386CONF <<EOF

ScrnInfoPtr x386Screens[] =
{
EOF
i=0
while [ $i -lt $NumScreens ]; do
  case $i in
    0)
      ScreenType=vga256;;
    1)
      ScreenType=vga2;;
    *)
      ScreenType=unknown;;
  esac
  Found=NO
  for j in $Args; do
    if [ X"$j" = X"$ScreenType" ]; then
      Found=YES
    fi
  done
  if [ X"$Found" = XYES ]; then
    Entry=\&${ScreenType}InfoRec
  else
    Entry=NULL
  fi
  i=`expr $i + 1`
  if [ $i -lt $NumScreens ]; then
    echo "        $Entry," >> $X386CONF
  else
    echo "        $Entry" >> $X386CONF
  fi
done
cat >> $X386CONF <<EOF
};

int     x386MaxScreens = sizeof(x386Screens) / sizeof(ScrnInfoPtr);
EOF
