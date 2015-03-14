include ../../../Configfile

PRGS=		bury cursor color font loadfont shape showfont square menu resetwin

all:

install:	all
		install -d $(BINDIR)
		install $(PRGS) $(BINDIR)
		for i in $(PRGS); do install -c -m 644 $$i.1 $(MANDIR)/man1; done

clean:

clobber:	clean
