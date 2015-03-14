include ../../../Configfile

CFLAGS=		$(FLAGS) -DSCREEN_DEV=\"SCREEN_DEV\"

PRGS=		browse cut cycle mclock walk window_print mgrbiff iconmail \
		iconmsgs bitmaptoc zoom snap mgrview

all:		$(PRGS)

browse:		browse.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

cut:		cut.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

cycle:		cycle.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

mclock:		mclock.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

walk:		walk.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

window_print:	window_print.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

mgrbiff:	mgrbiff.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

iconmail:	iconmail.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

iconmsgs:	iconmsgs.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

zoom:		zoom.o
		$(CC) $(LDFLAGS) -o $@ $@.o $(LIBDIR)/libmgr.a $(LIBDIR)/libbitblit.a

mgrview:	mgrview.o
		$(CC) $(LDFLAGS) -o $@ $@.o $(LIBDIR)/libmgr.a $(LIBDIR)/libbitblit.a

snap:		snap.o
		$(CC) $(LDFLAGS) -o $@ $@.o $(LIBDIR)/libmgr.a $(LIBDIR)/libbitblit.a

bitmaptoc:	bitmaptoc.o
		$(CC) $(LDFLAGS) -o $@ $@.o $(LIBDIR)/libbitblit.a

install-bitmaptoc:	bitmaptoc
		install -s bitmaptoc $(BINDIR)
		install -c -m 644 bitmaptoc.1 $(MANDIR)

install:        all
		install -s $(PRGS) $(BINDIR)
		for i in $(PRGS); do install -c -m 644 $$i.1 $(MANDIR)/man1; done

depend:
		$(CC) $(CFLAGS) -M *.c > dependencies

clean:
		rm -f *.o

clobber:	clean
		rm -f core $(PRGS)
		cat /dev/null > dependencies

include dependencies
