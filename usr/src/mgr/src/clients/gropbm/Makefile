include ../../../Configfile

CFLAGS=		$(FLAGS) -DHFONTDIR=\"$(HFONTDIR)\" -DMGR
LDFLAGS=

all:		gropbm widths

install:	all
		install -c -s gropbm $(BINDIR)
		mkdevpbm $(HFONTDIR) $(GROFFFONTDIR)
		install -c -m 644 gropbm.1 $(MANDIR)/man1

depend:
		gcc -M $(CFLAGS) *.c > dependencies

clean:
		rm -f core *.o

clobber:	clean
		cat /dev/null > dependencies
		rm -f gropbm widths fontview

gropbm:		gropbm.o bit.o hfont.o
		gcc -o $@ $(LDFLAGS) gropbm.o bit.o hfont.o

widths:		widths.c
		gcc -o $@ widths.c

fontview:	fontview.o $(LIBDIR)/libmgr.a
		gcc -o $@ $(LDFLAGS) fontview.o $(LIBDIR)/libmgr.a

include dependencies
