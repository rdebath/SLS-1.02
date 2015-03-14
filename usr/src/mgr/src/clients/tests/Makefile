include ../../../Configfile

CFLAGS=		$(FLAGS)

PRGS=		half many test_menu test_rop hfont spot

all:		$(PRGS)

%:		%.c $(LIBDIR)/libmgr.a
		$(CC) $(CFLAGS) -o $@ $@.c $(LIBDIR)/libmgr.a

install:	all

depend:
		$(CC) -M $(CFLAGS) *.c > dependencies

clean:

clobber:
		rm -f core $(PRGS)
		cat /dev/null > dependencies

include dependencies
