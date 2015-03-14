#
# dip		Dialup IP connection support program.
#		Makefile for LINUX (GCC 2.2.2d7).
#
# Version:	@(#)Makefile.LINUX	1.00	12/05/92	FvK
#
# Author:	Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
#		Copyright 1992 MicroWalt Corporation
#		This program is free software; you can redistribute it and/or
#		modify it under the terms of the GNU General Public License
#		as published by the Free Software Foundation; either version
#		2 of the License, or (at your option) any later version.
#

# GNU Gcc 2.2.2d PL7 with JumpTable 4.1
CC      = cc -c
LD      = cc  -nojump -s

# Compiler flags && compile options.
CFLAGS  = -O6

# Object modules.
OBJS	= dip.o serial.o ipdump.o route.o term.o modem.o \
	  command.o login.o p_slip.o p_ppp.o 

# Libraries.
LIBS	= #-linet #-lsyslog -lresolv #-linet


dip:		$(OBJS)
		$(LD) -o dip $(OBJS) $(LIBS)

clean:
		rm -f *.o core dip

dip.o:		dip.h dip.c
		$(CC) $(CFLAGS) -c dip.c

serial.o:	dip.h serial.c
		$(CC) $(CFLAGS) -c serial.c

ipdump.o:	dip.h ipdump.c
		$(CC) $(CFLAGS) -c ipdump.c

route.o:	dip.h route.c
		$(CC) $(CFLAGS) -c route.c

term.o:		dip.h term.c
		$(CC) $(CFLAGS) -c term.c

modem.o:	dip.h modem.c
		$(CC) $(CFLAGS) -c modem.c

command.o:	dip.h command.c
		$(CC) $(CFLAGS) -c command.c

login.o:	dip.h login.c
		$(CC) $(CFLAGS) -c login.c

p_slip.o:	dip.h p_slip.c
		$(CC) $(CFLAGS) -c p_slip.c

p_ppp.o:	dip.h p_ppp.c
		$(CC) $(CFLAGS) -c p_ppp.c

# End of Makefile.LINUX.
