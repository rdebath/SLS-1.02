include ../../../Configfile

CFLAGS=		$(FLAGS)

PRGS=		play_data to_ascii to_binary

all:		$(PRGS)

install:	all
		install -s $(PRGS) $(BINDIR)
		for i in $(PRGS) ; do install -c -m 644 $$i.1 $(MANDIR)/man1 ; done

play_data:	play_data.o
		$(CC) -o $@ play_data.o $(LIBDIR)/libbitblit.a

to_ascii:	to_ascii.o
		$(CC) -o $@ to_ascii.o $(LIBDIR)/libbitblit.a

clean:
		rm -f play_data.o

clobber:	clean
		rm -f $(PRGS)
