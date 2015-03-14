XCOMM!/bin/sh

XCOMM $Header: /home/x_cvs/mit/server/ddx/x386/LinkKit/mkmf.cpp,v 1.1 1992/09/02 12:02:04 dawes Exp $
XCOMM
XCOMM build Makefile for link kit
XCOMM
touch X386Conf.tmpl
(set -x; imake -I. CONFIGDIRSPEC -DUseInstalled -DInit -DX386LinkKit=1 -DTOPDIR=. -DCURDIR=.)
make Configure
