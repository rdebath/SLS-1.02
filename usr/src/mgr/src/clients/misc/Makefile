include ../../../Configfile

CFLAGS=		$(FLAGS)

PRGS=		bounce clock clock2 close grav grid mgrmsgs \
		hilbert mgrmail mgrmode oclose startup fonttoc \
		stringart set_termcap omgrmail maze \
		rotate show set_console

all:		$(PRGS)

bounce:		bounce.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

clock:		clock.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

clock2:		clock2.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

close:		close.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

grav:		grav.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

grid:		grid.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

mgrmsgs:	mgrmsgs.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

hilbert:	hilbert.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

mgrmail:	mgrmail.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

mgrmode:	mgrmode.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

oclose:		oclose.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

startup:	startup.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

fonttoc:	fonttoc.c
		$(CC) $(CFLAGS) -o $@ $@.c

rotate:		rotate.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libbitblit.a

show:		show.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU) $(LIBDIR)/libbitblit.a

set_console:	set_console.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

stringart:	stringart.o art_data.o
		$(CC) -o $@ stringart.o art_data.o $(LIBDIR)/libmgr.a $(LIBEMU)

set_termcap:	set_termcap.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

omgrmail:	omgrmail.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

maze:		maze.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a $(LIBEMU)

install-fonttoc:	fonttoc
		install -s fonttoc $(BINDIR)
		install -c -m 644 fonttoc.1 $(BINDIR)

install:        all
		install -d $(BINDIR)
		install -s $(PRGS) $(BINDIR)
		install -c -m 644 *.1 $(MANDIR)/man1

clean:
		rm -f stringart.o art_data.o

clobber:	clean
		rm -f core $(PRGS)
