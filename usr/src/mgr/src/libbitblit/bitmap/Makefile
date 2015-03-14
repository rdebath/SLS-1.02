include ../../../Configfile

CFLAGS=		$(FLAGS)
OBJS=		bitmaphead.o bitmapread.o bit_create.o bit_alloc.o

all:		$(OBJS)

$(OBJS):	screen.h

screen.h:
		ln -s ../$(BITBLIT)/$(SCREEN).h screen.h

install:	all

depend:		screen.h
		$(CC) -M $(CFLAGS) *.c > dependencies

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f screen.h
		cat /dev/null > dependencies

include dependencies
