include ../../../Configfile

CFLAGS=		$(FLAGS)

all:		ify setname

install:	all
		install -d $(BINDIR)
		install -s ify setname $(BINDIR)
		install -m 644 setname.1 ify.1 $(MANDIR)/man1

ify:		ify.o loadicon.o
		$(CC) -o $@ ify.o loadicon.o $(LIBDIR)/libmgr.a $(LIBDIR)/libbitblit.a

setname:	setname.c
		$(CC) $(CFLAGS) -o setname setname.c $(LIBDIR)/libmgr.a

loadicon.o:	icondata.h

icondata.h:	ify_icon
		$(BINDIR)/bitmaptoc ify_icon > icondata.h

clean:
		rm -f ify.o loadicon.o icondata.h

clobber:	clean
		rm -f setname ify
