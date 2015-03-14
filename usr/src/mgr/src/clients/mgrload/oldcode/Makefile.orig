# Makefile for mgrload
#	Mark Dapoz  90/06/21

CC	= cc
CFLAGS	= -O -DSYSV -DSAMPLES=120 -DINTERVAL=60 -DPSIZE=50
DEST	= /usr/local/bin
MGRLIB	= /usr/mgr/lib/libmgr.a

all: mgrload
	$(CC) $(LDFLAGS) mgrload.o -o mgrload $(MGRLIB)

mgrload: loadavgd mgrload.o

loadavgd: loadavgd.o
	$(CC) $(LDFLAGS) loadavgd.o -o loadavgd -lm

install: mgrload
	@if test `id | awk '{ FS="="; split($$2, n, "("); print n[1] }'` != "0" ; then echo "\n*** Must be root to install!"; exit 1 ; fi
	if test ! -d /etc/daemons ; then mkdir /etc/daemons ; fi
	/bin/cp loadavgd /etc/daemons
	/etc/daemons/loadavgd
	/bin/cp mgrload $(DEST)
	/bin/cp mgrload.1 /usr/man/man1

shar:
	shar -b -c -t -v README Makefile mgrload.c loadavgd.c \
		term.h window.h mgrload.1 > mgrload.shar

clean: 
	/bin/rm mgrload.o mgrload loadavgd.o loadavgd

clobber: clean
