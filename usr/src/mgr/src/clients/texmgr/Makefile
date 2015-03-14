include ../../../Configfile
include Configfile

CFLAGS=		$(FLAGS) -Ilibtex $(TEXMGRFLAGS)

OBJS=		texmgr.o misc.o libtex/libtex.a

all:		texmgr

texmgr:		$(OBJS)
		$(CC) $(LDFLAGS) -o texmgr $(OBJS) $(LIBDIR)/libmgr.a

texmgr.o:	libtex/dvistuff.h

libtex/libtex.a:
		cd libtex; make

install:	texmgr
		install -m 755 texmgr $(BINDIR)
		install -m 644 texmgr.1 $(MANDIR)/man1

clean:
		rm -f $(OBJS)
		(cd libtex; make clean)

clobber:	clean
		rm -f texmgr
		(cd libtex; make clobber)
