#                        Copyright (c) 1987 Bellcore
#                            All Rights Reserved
#       Permission is granted to copy or use this program, EXCEPT that it
#       may not be sold for profit, the copyright notice must be reproduced
#       on copies, and credit should be given to Bellcore where it is due.
#       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.

#	$Header: Makefile,v 4.5 88/08/22 10:30:44 sau Exp $
#	$Source: /tmp/mgrsrc/demo/misc/RCS/Makefile,v $

# makefile for demos

LIB= $(HOME)/mgr/lib$(ARCHITECTURE)/libmgr.a
BIT= $(HOME)/mgr/lib$(ARCHITECTURE)/blitlib.a
INCL= -I$(HOME)/mgr/include
CPIO=demo.cpio
TAR = demo.tar
CFLAGS=-O $(INCL)
LDFLAGS=$(LIB)
START=.
CPIO=misc.cpio
#INSROOT=/usr/mgr
INSROOT=$(HOME)/mgr
INSDIR=$(HOME)/bin$(ARCHITECTURE)

CFILES= art_data.c bounce.c clock.c clock2.c close.c \
	dmgr.c ether.c getpty.c grav.c grid.c hilbert.c hpmgr.c maze.c \
	mgrmail.c mgrmode.c mgrmsgs.c oclose.c omgrmail.c overlayd.c \
	set_console.c set_emacs.c set_termcap.c show.c startup.c \
	stat.c stringart.c  lock.c

OFILES= art_data.o bounce.o clock.o clock2.o close.o \
	dmgr.o ether.o getpty.o grav.o grid.o hilbert.o hpmgr.o maze.o \
	mgrmail.o mgrmode.o mgrmsgs.o oclose.o omgrmail.o overlayd.o \
	set_console.o set_emacs.o set_termcap.o show.o startup.o \
	stat.o stringart.o lock.o

SMALL=	bounce clock clock2 close dmgr ether grav grid hilbert \
	hpmgr maze mgrmail mgrmode mgrmsgs oclose omgrmail \
	set_emacs set_termcap show startup stat stringart

ALL = $(SMALL) overlayd set_console lock

OTHER = Makefile

all:		$(ALL)

small:		$(SMALL)

fast:
		cc -c $(CFLAGS) $(CFILES)
		make all

banner:		banner.o $(LIB)
		cc $(CFLAGS) -o banner banner.o $(LDFLAGS)

clock:		clock.o $(LIB)
		cc $(CFLAGS) -o clock clock.o $(LDFLAGS)

clock2:		clock2.o $(LIB)
		cc $(CFLAGS) -o clock2 clock2.o $(LDFLAGS)

clock3:		clock3.o $(LIB)
		cc $(CFLAGS) -o clock3 clock3.o $(LDFLAGS)

bounce:		bounce.o $(LIB)
		cc $(CFLAGS) -o bounce bounce.o $(LDFLAGS)

dmgr:		dmgr.o $(LIB)
		cc $(CFLAGS) -o dmgr dmgr.o $(LDFLAGS)

grav:		grav.o $(LIB)
		cc $(CFLAGS) -o grav grav.o $(LDFLAGS)

hpmgr:		hpmgr.o getpty.o $(LIB)
		cc $(CFLAGS) -o hpmgr hpmgr.o getpty.o $(LDFLAGS)

mgrmsgs:	mgrmsgs.o $(LIB)
		cc $(CFLAGS) -o mgrmsgs mgrmsgs.o $(LDFLAGS)

omgrmail:	omgrmail.o $(LIB)
		cc $(CFLAGS) -o omgrmail omgrmail.o $(LDFLAGS)

mgrmail:	mgrmail.o $(LIB)
		cc $(CFLAGS) -o mgrmail mgrmail.o $(LDFLAGS)

chart:		chart.o $(LIB)
		cc $(CFLAGS) -o chart chart.o $(LDFLAGS)

close:		close.o $(LIB)
		cc $(CFLAGS) -o close close.o $(LDFLAGS)

oclose:		oclose.o $(LIB)
		cc $(CFLAGS) -o oclose oclose.o $(LDFLAGS)

close3:		close3.o $(LIB)
		cc $(CFLAGS) -o close3 close3.o $(LDFLAGS)

stringart:	stringart.o art_data.o $(LIB)
		cc $(CFLAGS) -o stringart stringart.o art_data.o $(LDFLAGS)

stat2:		stat2.o $(LIB)
		cc $(CFLAGS) -o stat2 stat2.o $(LDFLAGS)

stat:		stat.o $(LIB)
		cc $(CFLAGS) -o stat stat.o $(LDFLAGS)

ether:		ether.o $(LIB)
		cc $(CFLAGS) -o ether ether.o $(LDFLAGS)

ether2:		ether2.o $(LIB)
		cc $(CFLAGS) -o ether2 ether2.o $(LDFLAGS)

set_console:	set_console.o $(LIB)
		cc $(CFLAGS) -o set_console set_console.o $(LDFLAGS)

set_emacs:	set_emacs.o $(LIB)
		cc $(CFLAGS) -o set_emacs set_emacs.o $(LDFLAGS)

set_termcap:	set_termcap.o $(LIB)
		cc $(CFLAGS) -o set_termcap set_termcap.o $(LDFLAGS)

mgrmode:	mgrmode.o $(LIB)
		cc $(CFLAGS) -o mgrmode mgrmode.o $(LDFLAGS)

#show:		show.o $(LIB)
#		cc $(CFLAGS) -o show show.o $(LDFLAGS)

startup:	startup.o $(LIB)
		cc $(CFLAGS) -o startup startup.o $(LDFLAGS)

hilbert:	hilbert.o $(LIB)
		cc $(CFLAGS) -o hilbert hilbert.o $(LDFLAGS)

overlayd:	overlayd.o $(LIB)
		cc $(CFLAGS) -o overlayd overlayd.o $(LDFLAGS)

grid:		grid.o $(LIB)
		cc $(CFLAGS) -o grid grid.o $(LDFLAGS)

view:		view.o $(LIB)
		cc $(CFLAGS) -o view view.o $(LDFLAGS)

file:		file.o $(LIB)
		cc $(CFLAGS) -o file file.o $(LDFLAGS)

print:		print.o $(LIB)
		cc $(CFLAGS) -o print print.o $(LDFLAGS)

wprint:		wprint.o $(LIB)
		cc $(CFLAGS) -o wprint wprint.o $(LDFLAGS)

lock:		lock.o $(LIB)
		cc $(CFLAGS) -o lock lock.o $(BIT) $(OTHERLIB)

maze:		maze.o $(LIB)
		cc $(CFLAGS) -o maze maze.o $(LDFLAGS)

$(OFILES):	$(HOME)/mgr/include/term.h $(LIB)

install:	all $(INSDIR)
		cd $(INSDIR);  rm -f $(ALL)
		cp $(ALL) $(INSDIR)

smallinstall:	small $(INSDIR)
		cd $(INSDIR);  rm -f $(SMALL)
		cp $(SMALL) $(INSDIR)
		-/etc/chown root $(INSDIR)/maze  &&  chmod u+s $(INSDIR)/maze

$(INSDIR):
	mkdir $@

tar:
		tar cf $(TAR) `ls $(CFILES) $(SH) $(OTHER)`

clean:
		rm -f *.o

clobber:
		rm -f $(ALL)

list:
	@for i in ${CFILES} ${HFILES} ${OTHER}; do \
		echo "${START}/$$i"; \
	done

cpio:
	make -s list | cpio -ocv > ${CPIO}
