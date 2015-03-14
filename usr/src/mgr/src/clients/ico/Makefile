include ../../../Configfile

CFLAGS=		$(FLAGS)

all:		ico

install:	all
		install -s ico $(BINDIR)
		install -m 644 ico.1 $(MANDIR)/man1

ico:		ico.o
		$(CC) -o $@ ico.o -lm $(LIBDIR)/libmgr.a

clean:
		rm -f ico.o

clobber:	clean
		rm -f core ico
