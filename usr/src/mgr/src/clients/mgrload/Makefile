include ../../../Configfile

CFLAGS=		$(FLAGS)

OBJS=		mgrload.o getload.o

all:		mgrload

mgrload:	$(OBJS)
		$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBDIR)/libmgr.a

getload.c:
		ln -s `uname`.c getload.c

install:        mgrload
		install -s mgrload $(BINDIR)
		install -c -m 644 mgrload.1 $(MANDIR)/man1

depend:		getload.c
		$(CC) -M $(CFLAGS) mgrload.c getload.c > dependencies

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f core mgrload getload.c
		cat /dev/null > dependencies

include dependencies
