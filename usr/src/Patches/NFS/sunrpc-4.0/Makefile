#
# @(#)Makefile	2.1 88/08/11 4.0 RPCSRC
#
#   Build and install everything.
#
# These directories are presumed to exist in DESTDIR:
#    /usr/lib /usr/bin /usr/include
#
DESTDIR=
CFLAGS=-O2
MAKE=make

SUBDIR= rpcgen etc rpcsvc

all install:	rpclib ${SUBDIR}

rpclib:	FRC
	@echo "Building and installing RPC library"
	cd rpc; $(MAKE) ${MFLAGS} all DESTDIR=${DESTDIR}; \
        $(MAKE) ${MFLAGS} install DESTDIR=${DESTDIR}

${SUBDIR}:	FRC
	@echo "Building and installing files from: $@"
	cd $@; $(MAKE) ${MFLAGS} DESTDIR=${DESTDIR} CFLAGS=${CFLAGS};\
        $(MAKE) ${MFLAGS} install DESTDIR=${DESTDIR}

FRC:

