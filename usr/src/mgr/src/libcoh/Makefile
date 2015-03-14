include ../../Configfile

CFLAGS=		$(FLAGS)
OBJS=		cfsetispeed.o gettimeofday.o select.o

all:		libcoh.a

libcoh.a:	$(OBJS)
		rm -f $@
		ar qv $@ $(OBJS)

install:	libcoh.a
		install libcoh.a $(LIBDIR)
		ranlib $(LIBDIR)/libcoh.a

clean:
		rm -f $(OBJS)

clobber:	clean
		rm -f libcoh.a
