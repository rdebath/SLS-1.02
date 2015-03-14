include ../../../Configfile

CFLAGS=		$(FLAGS)

all:		lines monorop rops

depend:
		gcc -M $(CFLAGS) *.c > dependencies

monorop:	monorop.o $(LIBDIR)/libbitblit.a
		$(CC) $(LDFLAGS) -o $@ monorop.o $(LIBDIR)/libbitblit.a

lines:		lines.o $(LIBDIR)/libbitblit.a
		$(CC) $(LDFLAGS) -o $@ lines.o $(LIBDIR)/libbitblit.a

rops:		rops.o $(LIBDIR)/libbitblit.a
		$(CC) $(LDFLAGS) -o $@ rops.o $(LIBDIR)/libbitblit.a

clean:
		rm -f lines.o lines.log monorop.o rops.o analyse.o

clobber:	clean
		rm -f core lines monorop rops
		cat /dev/null > dependencies

include dependencies
