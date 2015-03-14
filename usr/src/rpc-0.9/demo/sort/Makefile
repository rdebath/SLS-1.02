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
# @(#)Makefile	2.6 91/03/11 4.0 RPCSRC
#
CC = gcc
BIN =  rsort sort_svc
GEN = sort_clnt.c sort_svc.c sort_xdr.c sort.h
LIB =
LDFLAGS=
RPCCOM = ../../rpcgen/rpcgen -I
CFLAGS = -g

all: $(BIN)

rsort: rsort.o sort_clnt.o sort_xdr.o
	$(CC) $(LDFLAGS) -o $@ rsort.o sort_clnt.o sort_xdr.o $(LIB)

rsort.o: rsort.c sort.h

sort_clnt.c:
	$(RPCCOM) -l sort.x >$@

sort_svc: sort_proc.o sort_svc.o sort_xdr.o
	$(CC) $(LDFLAGS) -o $@ sort_proc.o sort_svc.o sort_xdr.o $(LIB)

sort_proc.o: sort_proc.c sort.h

sort_svc.c:
	$(RPCCOM) -s udp sort.x >$@

sort_xdr.c:
	$(RPCCOM) -c sort.x >$@

sort.h:
	$(RPCCOM) -h sort.x >$@

clean:
	rm -f $(GEN) *.o

clobber: clean
	rm -f $(BIN)

