# Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)
# 
# Permission to use, copy, modify, and distribute this material 
# for any purpose and without fee is hereby granted, provided 
# that the above copyright notice and this permission notice 
# appear in all copies, and that the name of Bellcore not be 
# used in advertising or publicity pertaining to this 
# material without the specific, prior written permission 
# of an authorized representative of Bellcore.  BELLCORE 
# MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
# OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
# WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
#

FOOBAR-sun4=-Bstatic

STATICFLAG=FOOBAR${HOST_ARCH}

CFLAGS = -g -I.  ${$(STATICFLAG)} 

LDLIBS = 

CONFIGDIR = ..

LOCALCFLAGS= -I${CONFIGDIR} ${CFLAGS}

.c.o:
	rm -f $@
	cc -c  $(LOCALCFLAGS) $*.c

ezview: ezview.o unscribe.o ulstrcmp.o
	cc ${LOCALCFLAGS} -o ezview ezview.o unscribe.o ulstrcmp.o $(LDLIBS)

clean:
	-rm -f ezview *.o *.BAK
