CC=		gcc
CFLAGS=		-O6 -Wall -pipe
LDFLAGS=	-O6 -s -N
PROGS=		dirdump e2fsck mke2fs # convertefs
MANPAGES=	mke2fs.8 e2fsck.8
BINDIR=		/bin
ETCDIR=		/etc
MANDIR=		/usr/man/man8

all:		$(PROGS)

clean:
	rm -f $(PROGS) *.s *.o *.a *~ core

install:	$(PROGS) $(MANPAGES)
	cp $(PROGS) $(BINDIR)
	ln -sf $(BINDIR)/mke2fs $(ETCDIR)/mkfs.ext2
	ln -sf $(BINDIR)/e2fsck $(ETCDIR)/fsck.ext2
	if [ -d $(MANDIR) ]; then \
		cp $(MANPAGES) $(MANDIR); \
		ln -sf $(MANDIR)/mke2fs.8 $(MANDIR)/mkfs.ext2.8; \
		ln -sf $(MANDIR)/e2fsck.8 $(MANDIR)/fsck.ext2.8; \
	fi

dep:
	$(CPP) -M *.c >.depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
