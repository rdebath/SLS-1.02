# Makefile -- Makefile for NET-2 utilties ported to Linux
# Created: Sun Jun 14 13:43:28 1992
# Revised: Sat Jan 16 17:51:59 1993 by root
# Copyright 1992 Rickard E. Faith (faith@cs.unc.edu)
#
# $Log$
#

CC= gcc
CFLAGS= -O6 -I.
LDFLAGS= -s -N

MAN1= cal.1 chsh.1 clear.1 col.1 colcrt.1 colrm.1 column.1 hexdump.1 \
	last.1 mesg.1 od.1 passwd.1 reset.1 script.1 strings.1 ul.1 \
	uuencode.1 whereis.1
MAN5= uuencode.5
MAN6= banner.6

OTHER= banner cal chsh col colcrt colrm column hexdump last mesg \
	passwd strings uudecode uuencode whereis
NEEDS_TERMCAP= ul
SCRIPTS= clear reset
BINS= $(OTHER) $(SCRIPTS) $(NEEDS_TERMCAP) script

all: $(BINS)

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

$(NEEDS_TERMCAP):
	$(CC) $(LDFLAGS) $^ -o $@ -ltermcap

$(OTHER):
	$(CC) $(LDFLAGS) $^ -o $@

$(SCRIPTS):
	cp $@.sh $@

script: script.c paths.h fake_wait.h cfmakeraw.o
	$(CC) $(CFLAGS) $(LDFLAGS) -include fake_wait.h -o script \
		script.c cfmakeraw.o

banner: banner.o getopt.o
cal: cal.o getopt.o
chsh: chsh.o
clear: clear.sh
col: col.o getopt.o
colcrt: colcrt.o
colrm: colrm.o
column: column.o getopt.o
hexdump: hexdump.o getopt.o conv.o display.o hexsyntax.o odsyntax.o parse.o
last: last.o
mesg: mesg.o
passwd: passwd.o
reset: reset.sh
strings: strings.o getopt.o
ul: ul.o getopt.o
uudecode: uudecode.o
uuencode: uuencode.o getopt.o
whereis: whereis.o

hexdump.o: hexdump.c hexdump.h
conv.o: conv.c hexdump.h
display.o: display.c hexdump.h
hexsyntax.o: hexsyntax.c hexdump.h
odsyntax.o: odsyntax.c hexdump.h
parse.o: parse.c hexdump.h

install: all
	rm -f /usr/bin/uudecode
	rm -f /usr/bin/od
	install -m 755 $(BINS) /usr/bin
	ln -sf /usr/bin/hexdump /usr/bin/od
	chown root /usr/bin/passwd
	chmod u+s /usr/bin/passwd
	install -m 644 $(MAN1) /usr/man/man1
	ln -sf /usr/man/man1/uuencode.1 /usr/man/man1/uudecode.1
	install -m 644 $(MAN5) /usr/man/man5
	install -m 644 $(MAN6) /usr/man/man6
	
clean:
	-rm -f $(BINS) *.o *~
