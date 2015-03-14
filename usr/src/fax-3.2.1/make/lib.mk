#  lib.mk
#
#   (c) Copyright 1992 by David M. Siegel.
#       All rights reserved.
#
#  $Id$

ARNAME= lib$(LIBNAME).a

all: $(ARNAME)

$(OBJS:%=$(ARNAME)(%)): $(ARNAME)(%.o): %.c
	$(CC) $(CFLAGS) -c $% $<
	ar rv $(ARNAME) $%
	rm -f $%

$(ARNAME): $(OBJS:%=$(ARNAME)(%))
	ranlib $@

clean:
	$(RM) $(OBJS) $(ARNAME)

world: all
