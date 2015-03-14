include ../../../Configfile

CFLAGS=		$(FLAGS)

OBJS=		hpmgr.o getpty.o

all:		hpmgr

hpmgr:		$(OBJS)
		$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBDIR)/libmgr.a

install:        all
		install -s hpmgr $(BINDIR)
		install -c -m 644 hpmgr.1 $(MANDIR)/man1

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f core hpmgr
