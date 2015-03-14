include ../../../Configfile

CFLAGS=		$(FLAGS)

PRGS=		font2text text2font

all:		$(PRGS)

%:		%.c
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

install:        all
		install -s $(PRGS) $(BINDIR)
		for i in $(PRGS); do install -c -m 644 $$i.1 $(MANDIR)/man1; done

clean:

clobber:
		rm -f core $(PRGS)
