include ../../Configfile

CFLAGS=		$(FLAGS) -DHFONTDIR=\"$(HFONTDIR)\"
OBJS=		ckmgrterm.o term.o hfont.o

all:		libmgr.a

libmgr.a:	$(OBJS)
		rm -f $@
		ar qv $@ $(OBJS)

install:	libmgr.a
		install libmgr.a $(LIBDIR)
		ranlib $(LIBDIR)/libmgr.a

depend:
		$(CC) -M $(CFLAGS) *.c > dependencies

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f libmgr.a
		cat /dev/null > dependencies

include dependencies
