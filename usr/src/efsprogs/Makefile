CC=		gcc
CFLAGS=		-O -Wall
LDFLAGS=	-s
PROGS=		efsck mkefs
MANPAGES=	efsck.8 mkefs.8
BINDIR=		/bin
MANDIR=		/usr/man/man8

all:		$(PROGS)

clean:
	rm -f $(PROGS) *.o *~ core

install:	$(PROGS)
	cp $(PROGS) $(BINDIR)
	if [ -d $(MANDIR) ]; then \
		cp $(MANPAGES) $(MANDIR); \
	fi

dep:
	$(CPP) -M *.c >.depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
