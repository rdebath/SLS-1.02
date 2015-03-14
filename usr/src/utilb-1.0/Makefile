# Makefile -- Makefile for utilb-1.0 Linux utilities
# Created: Fri Dec 25 15:57:18 1992
# Revised: Sun Dec 27 14:47:40 1992 by root
# Copyright 1992 Rickard E. Faith (faith@cs.unc.edu)
#
# $Log$
#

CC=        gcc
CFLAGS=    -O6 -I.
LDFLAGS=   -s -N

MAN1= ed.1 hostname.1 kill.1 ldd.1 login.1 more.1 setterm.1 wall.1 write.1

LIB_FILES= more.help

OTHER= ed hostname kill ldd login wall write
NEEDS_TERMCAP=   more setterm
BIN= $(OTHER) $(NEEDS_TERMCAP)

all: $(BIN)

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@


$(NEEDS_TERMCAP):
	$(CC) $(LDFLAGS) $^ -o $@ -ltermcap

$(OTHER):
	$(CC) $(LDFLAGS) $^ -o $@

ed: ed.o
hostname: hostname.o
kill: kill.o
ldd: ldd.o
login: login.o
more: more.o
setterm: setterm.o
wall: wall.o dowall.o
write: write.o

more.o: more.c pathnames.h

install: all
	install -m 755 $(BIN) /bin
	install -m 644 $(LIB_FILES) /usr/lib
	install -m 644 $(MAN1) /usr/man/man1
	
clean:
	-rm -f $(BIN) *.o *~
