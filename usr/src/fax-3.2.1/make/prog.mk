#  prog.mk
#
#   (c) Copyright 1992 by David M. Siegel.
#       All rights reserved.
#
#  $Id$


all: $(PROGNAME)

$(PROGNAME): $(OBJS) $(DEPENDS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	$(RM) $(OBJS) $(PROGNAME) core

world: all
