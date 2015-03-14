include ../../../Configfile

CFLAGS=		$(FLAGS)
OBJS=		mgrplot.o subs.o

all:		mgrplot

install:	all
		install -s mgrplot $(BINDIR)
		install -m 644 mgrplot.1 $(MANDIR)/man1                

mgrplot:	$(OBJS)
		$(CC) -o $@ $(OBJS) $(LIBDIR)/libmgr.a

clean:
		rm -f $(OBJS)
                
clobber:	clean
		rm -f mgrplot
