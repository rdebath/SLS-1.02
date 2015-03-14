#
#*****************************************************************************
#									      *
#	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
#	copyright laws of the United States.			      	      *
#									      *
#*****************************************************************************
#

#
#	makefile for xmahjongg
#
XINC	= /usr/include
XLIB	= /usr/lib/libX11.a
BIN	= /usr/bin/X11
TMP	= /usr/lib/X11/xmahjongg
CFLAGS	= -O -I$(XINC) -DGLOBAL=extern -DLAYOUT=\"$(TMP)\"
BOARDS	= default bridge wedges
OBJS	= xmahjongg.o draw.o event.o initial.o packet.o play.o random.o \
	  sysdep.o variables.o

all:	xmahjongg

xmahjongg: $(OBJS)
	   $(CC) $(CFLAGS) -o xmahjongg $(OBJS) $(XLIB)

install: all
	 -mkdir $(TMP)
	 cp $(BOARDS) $(TMP)
	 cp xmahjongg $(BIN)
	 chmod 711 $(BIN)/xmahjongg
clean: 
	rm -f xmahjongg *.o

