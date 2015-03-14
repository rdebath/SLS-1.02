# Makefile for XC
# For XC, the following compiler switches are significant:
# 
# LINUX note: if you are using GCC before verson 1.40, you will have to
#             remove the "-ltermcap" flag from LDFLAGS. 
# 
#	-DDIDO=22
#	-DDIDO=23
#		For a Dial-In/Dial-Out port.
#		22 is for SCO Xenix 2.2 which uses ungetty.
#		23 is for SCO Xenix 2.3 whose getty is effectively a uugetty,
#		for SCO Unix, or for other sites using uugetty.
#
#	-DT6000
#		Causes the Tandy-6000 specific code modifications to be compiled.
#
#	-DDEBUG
#		If this compiler switch is set, then whenever XC is run within a
#		directory that contains a file called "debug.log", XC will append to
#		that file a copy of all XC output, whether to the terminal "file" or
#		to standard error. This can be useful for debugging scripts, coupled
#		with the "debug" script command, which causes the lines of a script
#		to be echoed to the terminal as they get executed. Since often such
#		output passes up the terminal screen too quickly to read, the
#		"debug.log" file provides a hard-copy of the XC session.
#		No echoing to a "debug.log" file is done if XC is also 	capturing
#		incoming text to a capture file. If no "debug.log" file exists in the
#		current directory, then no such echoing is done regardless of whether
#		the -DDEBUG switch was set at compile time.
#
#	-DNOSHELL
#		This will disallow shell escapes. Both the "!" and "$" mechanisms from
#		the XC prompt, and the analogous SHELL and PIPE keywords in scripts,
#		silently do nothing. However the standard output of backquoted shell
#		commands can still be assigned to a script variable.
#

SHELL = /bin/sh

# for debugging output
DEBUG = -DDEBUG

# to disallow shell escapes
# NOSHELL = -DNOSHELL

# choose your poison:
# for SCO Xenix 2.3 and SCO Unix
# CFLAGS	= -Ox -DDIDO=23 $(DEBUG) $(NOSHELL)
# for SCO Xenix 2.2
# CFLAGS	= -Ox -DDIDO=22 $(DEBUG) $(NOSHELL)
# for Tandy Xenix 3.2
# CFLAGS	= -O -n -DT6000 $(DEBUG) $(NOSHELL)
# for SCO Xenix on an IBM PC/AT (80286)
# CFLAGS	= -O -i $(DEBUG) $(NOSHELL)
# for "any" Unix or Xenix
CFLAGS	= -O $(DEBUG) $(NOSHELL)

# For SCO Xenix
# LDFLAGS = -ltermcap 
# For SCO Unix, include -lc_s
LDFLAGS = -ltermcap 

# choose a nice home for XC
INSDIR		=/pub/jpr/bin
OWNER		=uucp
GROUP		=bin
BINMODE		=4711
MANDIR		=/usr/man/man
CATMANDIR	=/usr/man/cat
MANEXT		=.L
MANOWN		=bin
MANGRP		=bin
MANMODE		=0444
SHRINK		=compress

# you should have all of these files:
MANIFEST= .autocis .uni .callhq .exrc .phonelist .xc Makefile README\
			myman xcb+.c xcdbglog.c xcdial.c xc.h xc.nro xcmain.c\
			xcport.c xcscrpt.c xcsubs.c xcterm.c xcxmdm.c

OBJS	= xcmain.o xcb+.o xcdbglog.o xcdial.o xcport.o xcscrpt.o xcsubs.o\
			xcterm.o xcxmdm.o

xc:	 $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o xc $(LDFLAGS) 

install: xc
	@ls -l xc
	-mv $(INSDIR)/xc $(INSDIR)/xc-
	cp xc $(INSDIR)/xc
	strip $(INSDIR)/xc
	chown $(OWNER) $(INSDIR)/xc
	chgrp $(GROUP) $(INSDIR)/xc
	chmod $(BINMODE) $(INSDIR)/xc
	-@ls -l $(INSDIR)/xc $(INSDIR)/xc-

man: xc.nro
	cp xc.nro $(MANDIR)$(MANEXT)/xc$(MANEXT)
	chown $(MANOWN) $(MANDIR)$(MANEXT)/xc$(MANEXT)
	chgrp $(MANGRP) $(MANDIR)$(MANEXT)/xc$(MANEXT)
	chmod $(MANMODE) $(MANDIR)$(MANEXT)/xc$(MANEXT)
	-$(SHRINK) $(MANDIR)$(MANEXT)/xc$(MANEXT)
	touch man

catman: xc.nro
	tbl xc.nro| nroff myman - | col > $(CATMANDIR)$(MANEXT)/xc$(MANEXT)
	chown $(MANOWN) $(CATMANDIR)$(MANEXT)/xc$(MANEXT)
	chgrp $(MANGRP) $(CATMANDIR)$(MANEXT)/xc$(MANEXT)
	chmod $(MANMODE) $(CATMANDIR)$(MANEXT)/xc$(MANEXT)
	-$(SHRINK) $(CATMANDIR)$(MANEXT)/xc$(MANEXT)
	touch catman

lint:
	lint $(CFLAGS) $(LDFLAGS) -p xc*.c | less

clean:
	rm -f $(OBJS) xc core

$(OBJS): xc.h xctolu.h

shar:
	shar -a README .autocis .uni .callhq .exrc .phonelist .xc Makefile\
		xc.h xcdbglog.c xcsubs.c > xc1.shar
	shar -a xcb+.c xcport.c myman> xc2.shar
	shar -a xc.nro > xc3.shar
	shar -a xcscrpt.c > xc4.shar
	shar -a xcdial.c xcmain.c xcterm.c xcxmdm.c > xc5.shar
	@ls -l *.shar

taz:
	tar cf - $(MANIFEST) | compress > XC.TAR.Z
	@ls -l *Z

lzh:
	lharc c XC $(MANIFEST)

shark:
	shark $(MANIFEST) > XC.SHK
	@ls -l *SHK

mail:
	shark README .phonelist .xc .uni xc.h xcmain.c xcdial.c\
		xcport.c xcsubs.c xcdbglog.c > XC.SHARK1
	shark myman xcb+.c xcscrpt.c > XC.SHARK2
	shark Makefile xcterm.c xcxmdm.c xc.nro .autocis .exrc .callhq > XC.SHARK3
	@ls -l *SHARK*
