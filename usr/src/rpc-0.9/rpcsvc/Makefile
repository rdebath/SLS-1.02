#
# Sun RPC is a product of Sun Microsystems, Inc. and is provided for
# unrestricted use provided that this legend is included on all tape
# media and as a part of the software program in whole or part.  Users
# may copy or modify Sun RPC without charge, but are not authorized
# to license or distribute it to anyone else except as part of a product or
# program developed by the user or with the express written consent of
# Sun Microsystems, Inc.
#
# SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
# WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
#
# Sun RPC is provided with no support and without any obligation on the
# part of Sun Microsystems, Inc. to assist in its use, correction,
# modification or enhancement.
#
# SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
# INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
# OR ANY PART THEREOF.
#
# In no event will Sun Microsystems, Inc. be liable for any lost revenue
# or profits or other special, indirect and consequential damages, even if
# Sun has been advised of the possibility of such damages.
#
# Sun Microsystems, Inc.
# 2550 Garcia Avenue
# Mountain View, California  94043
#
#
# @(#)Makefile 1.27 91/03/11 TIRPC 1.0
#
# Copyright (c) 1988 by Sun Microsystems, Inc.
#
# To install librpcsvc.a
#

DESTDIR = /usr
CC	= gcc
RM	= rm -f
LD      = ld
CP	= cp
LINT	= lint
DESTLIB	= $(DESTDIR)/lib
DESTHDR = $(DESTDIR)/include/rpcsvc
LIBS	= librpcsvc.a
INC	= -I.. -I$(DESTDIR)/include
CPPFLAGS= $(INC) -DYP
CFLAGS	= -O2
RPCCOM	= ../rpcgen/rpcgen
LORDER	= ../lorder/lorder
TSORT	= ../tsort/tsort
AR	= ar
RANLIB	= ranlib

RPCSVCSOURCES =  bootparam_prot.x klm_prot.x mount.x nfs_prot.x nlm_prot.x \
		 rpcb_prot.x rex.x rnusers.x rstat.x rwall.x sm_inter.x  \
		 spray.x yp.x yppasswd.x ypupdate_prot.x
RPCSVCHDRS= bootparam.h ether.h ipalloc.h klm_prot.h mount.h nfs_prot.h \
		 nlm_prot.h pnprpc.h pwdnm.h rpcb_prot.h rex.h \
		 rusers.h rnusers.h rstat.h rwall.h showfh.h \
		 sm_inter.h spray.h ypclnt.h yp_prot.h yppasswd.h \
		 ypupdate_prot.h yp_prot.h
GENHDRS = $(RPCSVCSOURCES:.x=.h) bootparam.h 
RPCSVCXDR= rnusers_xdr.c rstat_xdr.c spray_xdr.c
RPCSVCTMP= $(RPCSVCXDR)
RPCSVCHIGH=rstat.c rwall.c rnusers.c

SECRPCSOURCES = # publickey.c secretkey.c xcrypt.c

SOURCES=        $(RPCSVCXDR) $(SECRPCSOURCES) $(RPCSVCHIGH)
OBJECTS=        $(SOURCES:.c=.o)

all: $(RPCSVCHDRS) librpcsvc.a

librpcsvc.a: $(OBJECTS)
	$(AR) rv $@ `$(LORDER) *.o | $(TSORT)`
	$(RANLIB) $@

#
# All the headers have been generated and SCCS-controlled.  Therefore,
# we do not need to generate header files everytime.
# dct, 9/5/90
#
# unfortunately they were not the ANSI C and C++ compatible type -- jrs

.SUFFIXES: .x
.x.h:
	$(RPCCOM) -h $< > $@

bootparam.h: bootparam_prot.x
	$(RPCCOM) -h $< > $@

rnusers_xdr.c:	rnusers.x
	$(RPCCOM) -c rnusers.x | \
	sed 's/^#include \"$*\.h\"/#include <rpcsvc\/$*\.h>/' > $@

rnusers_xdr.o:	rnusers.h

rstat_xdr.c:	rstat.x
	$(RPCCOM) -c rstat.x | \
	sed 's/^#include \"$*\.h\"/#include <rpcsvc\/$*\.h>/' > $@

rstat_xdr.o:	rstat.h

spray_xdr.c:	spray.x
	$(RPCCOM) -c spray.x | \
	sed 's/^#include \"$*\.h\"/#include <rpcsvc\/$*\.h>/' > $@

spray_xdr.o:	spray.h

rwall.o:	rwall.h

rstat.o:	rstat.h

rnusers.o:	rnusers.h

install:	$(RPCSVCHDRS) $(LIBS)
	if [ ! -d $(DESTDIR)/include ]; then\
		install -d -m 755 -o root -g staff $(DESTDIR)/include;\
	fi
	if [ ! -d $(DESTHDR) ]; then\
		install -d -m 755 -o root -g staff $(DESTHDR);\
	fi
	install -m 644 -o root -g staff $(RPCSVCHDRS) $(RPCSVCSOURCES) \
		$(DESTHDR)
	if [ ! -d $(DESTLIB) ]; then\
		install -d -m 755 -o root -g staff $(DESTLIB);\
	fi
	install -m 644 -o root -g staff librpcsvc.a $(DESTLIB)/librpcsvc.a
	ranlib $(DESTLIB)/librpcsvc.a


lint:
	$(LINT) $(CPPFLAGS) $(SOURCES)

clean:
	$(RM) $(OBJECTS) $(RPCSVCTMP)

clobber: clean
	$(RM) $(LIBS) $(GENHDRS)
