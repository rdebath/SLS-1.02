include ../../../Configfile

CFLAGS=		$(FLAGS)
OBJS=		share.o timestamp.o

all:		$(OBJS)

install:	all

depend:
		$(CC) -M $(CFLAGS) *.c > dependencies

clean:
		rm -f $(OBJS)

clobber:	clean
		cat /dev/null > dependencies
