# $Id: Makefile,v 1.7 1993/01/17 23:34:11 kernel Exp kernel $

BIN_DIR = /usr/bin
ATRUN_DIR = /usr/spool/atjobs
LOCKFILE = .lockfile
ATLIB_DIR = /usr/lib
PROC_DIR = /proc
MAIL_CMD = /bin/rmail
MAN_DIR = /usr/man
CFLAGS = -g -O2 -Wall -DATRUN_DIR=\"$(ATRUN_DIR)/\" -DPROC_DIR=\"$(PROC_DIR)/\"\
	-DMAIL_CMD=\"$(MAIL_CMD)\" -DLFILE=\"$(ATRUN_DIR)/$(LOCKFILE)\"
CSRCS = at.c atrun.c panic.c parsetime.c gloadavg.c
HEADERS = at.h atrun.h
DIST = Makefile README COPYING Changelog at.1 atrun.8 $(CSRCS) $(HEADERS)

.PHONY: all install clean dist

all: .depend at atrun

at: at.o panic.o parsetime.o

atrun: atrun.o gloadavg.o

install: at atrun
	install -g cron -o root -m 771 -d $(ATRUN_DIR)
	cat < /dev/null > $(ATRUN_DIR)/$(LOCKFILE)
	chmod 660 $(ATRUN_DIR)/$(LOCKFILE)
	chgrp cron $(ATRUN_DIR)/$(LOCKFILE)
	install -g cron -o root -m 2755 -s at $(BIN_DIR)
	ln -fs $(BIN_DIR)/at $(BIN_DIR)/batch
	install -g bin -o root -m 755 atrun $(ATLIB_DIR)
	install -g bin -o root -m 755 -d $(MAN_DIR)/man1
	install -g bin -o root -m 644 at.1 $(MAN_DIR)/man1
	ln -fs $(MAN_DIR)/man1/at.1 $(MAN_DIR)/man1/batch.1
	install -g bin -o root -m 755 -d $(MAN_DIR)/man8
	install -g bin -o root -m 644 atrun.8 $(MAN_DIR)/man8

.depend:
	$(CC) -M $(CSRCS) > .depend

dist: $(DIST)
	mv -f ../at ../reserve_at
	mkdir ../at
	cp $(DIST) ../at
	(cd ..; tar cf - at | compress > at1-1.tar.Z )
	rm -rf ../at
	mv -f ../reserve_at ../at

clean:
	rm -f *.o at atrun core

include .depend
