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
#	@(#)Makefile 1.14 89/03/30 (C) 1987 SMI <MODIFIED>
#
# Makefile for rpc protocol compiler
# Copyright (C) 1987, Sun Microsystems, Inc.
#

CC = gcc

CFLAGS = -O2 $(CPPFLAGS) -Dinline=inline_flag

STRIP = strip

SIZE = size

DIR = /usr/bin

INS = install

LIBS = # -lc

MAINS = rpcgen

OBJECTS = rpc_clntout.o rpc_cout.o rpc_hout.o rpc_main.o rpc_parse.o \
  	  rpc_scan.o rpc_svcout.o rpc_tblout.o rpc_util.o rpc_sample.o

all:	$(MAINS)

$(MAINS): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS)  $(LIBS)

clean:
	rm -f $(OBJECTS)

clobber:
	rm -f $(OBJECTS) $(MAINS)

install: all
	@if [ ! -d $(DIR) ] ; \
	then \
		mkdir $(DIR); \
	fi
	$(INS) $(MAINS) $(DIR)

size: all
	$(SIZE) $(MAINS)

strip: all
	$(STRIP) $(MAINS)
