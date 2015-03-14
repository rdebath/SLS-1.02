# Makefile for tin - for tin compiler flag options read INSTALL and README.
#
# Options that may need changing. For advanced options read the INSTALL file.
# 
CC	= cc
COPTS	= -c -O
#CC	= gcc
#COPTS	= -c -O2 -Wall -Wshadow
# LD has to be changed to ln for AmigaDOS
LD	= $(CC)
DEBUG	=
#DEBUG	= -g -DDEBUG
BINDIR	= /usr/local/bin
MANDIR	= /usr/local/man/man
MANEXT	= 1
LIBDIR	= /usr/lib/news
SPOOLDIR= /usr/spool/news
AMIGA_COPTS=-so -wc -m0s -DAMIGA -DSYSV -DDONT_REREAD_ACTIVE_FILE \
		-DSMALL_MEMORY_MODEL -DNO_SHELL_ESCAPE -DNO_PIPING \
		-DSLOW_SCREEN_UPDATE -DAMIGA_BBS -DLIBDIR=""uulib:"" \
		-DSPOOLDIR=""uunews:"" -DDONT_HAVE_SELECT
ANL_INEWS_GATEWAY=%anl433.uucp@Germany.EU.net
NNTP_INEWS_GATEWAY=
NNTP_INEWS_DOMAIN=


# Define if you want the multi SPOOLDIR extensions & library
CDROM	= -DCDROM_ABLE
# sparc10_1
CDLIB	= -L/home/iain/.src/cllib/clientlib -lcllib -ldbm
# anl433
#CDLIB	= /optdisk/.ADMIN_SRC/localsrc/cnews-911222/dbz/dbz.o -L/optdisk/.ADMIN_SRC/iain/cllib/clientlib -lcllib -lcposix

PROJECT	= tin
EXE	= tin
EXED	= tind
MAKE	= make
SHELL	= /bin/sh
STRIP	= strip
STRIP2	= mcs -d
ROFF	= nroff -man 
ROFF	= groff -Tascii -man 
BASE_VER= 1.17
VER = 1.18
MAIL_ADDR = "Iain.Lea%anl433.uucp@Germany.EU.net (Iain Lea)"

HFILES	= config.h tin.h extern.h nntplib.h proto.h amiga.h

CFILES	= active.c amiga.c art.c curses.c debug.c envarg.c feed.c getline.c \
		group.c hashstr.c help.c inews.c init.c kill.c lang.c mail.c \
		main.c memory.c misc.c newsrc.c nntplib.c open.c page.c \
		parsedate.y post.c prompt.c rcfile.c save.c screen.c search.c \
		select.c sigfile.c signal.c spooldir.c strftime.c thread.c \
		wildmat.c

OFILES	= active.o amiga.o art.o curses.o debug.o envarg.o feed.o getline.o \
		group.o hashstr.o help.o inews.o init.o kill.o lang.o mail.o \
		main.o memory.o misc.o newsrc.o nntplib.o open.o page.o \
		parsedate.o post.o prompt.o rcfile.o save.o screen.o search.o \
		select.o sigfile.o signal.o spooldir.o strftime.o thread.o \
		wildmat.o 

SUPPORT = Makefile MANIFEST README README.AMIGA CHANGES INSTALL TODO FTP *.[13] \
		$(EXE).nrf kill actived.c

NNTP_PATCH = README.NNTP INSTALL.NNTP common.patch server.patch xindex.c \
		xmotd.c xoverview.c xuser.c

ALL_FILES = $(SUPPORT) $(NNTP_PATCH) $(HFILES) patchlev.h $(CFILES)

LINTFLAGS=-a -c -h -n -x 

all: 
	@echo "Makefile for the TIN v$(VER) Netnews newsreader. Configuration:"
	@echo " "
	@echo "    Make=[$(MAKE)]  Compiler=[$(CC)]  Copts=[$(COPTS)]  Debug=[$(DEBUG)]"
	@echo "    Install Bindir=[$(BINDIR)]  Mandir=[$(MANDIR)$(MANEXT)]  Manext=[$(MANEXT)]"
	@echo "    News Libdir=[$(LIBDIR)]  Spooldir=[$(SPOOLDIR)]"
	@echo "    News Gateway=[$(NNTP_INEWS_GATEWAY)]  Domain=[$(NNTP_INEWS_DOMAIN)]"
	@echo " "
	@echo "If the above options are OK make one of the following targets:"
	@echo " "
	@echo "    make aix       [ IBM AIX ]"
	@echo "    make amiga     [ AmigaDOS ]"
	@echo "    make apollo    [ Apollo DomainOS ]"
	@echo "    make bsd       [ BSD/BSDI/DG/NeXT/Pyramid/SunOS/Ultrix ]"
	@echo "    make dynix     [ Sequent DYNIX ]"
	@echo "    make irix      [ SGI Irix ]"
	@echo "    make minix     [ Minix 386 ]"
	@echo "    make ptx       [ Sequent PTX ]"
	@echo "    make sco       [ SCO Unix ]"
	@echo "    make sinix     [ SNI Sinix ]"
	@echo "    make sysv      [ SysV / HPUX 7.0]"
	@echo "    make sysvr4    [ SysV R4 / HPUX 8.0 & 9.0]"
	@echo "    make tower     [ NCR Tower ]"
	@echo "    make xenix     [ Xenix 386 ]"

.c.o:
	$(CC) $(CFLAGS) $*.c

# For IBM AIX
aix:
	@echo "Compiling $(EXE) v$(VER) for IBM AIX..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSYSV -DRS6000 -DREAD_CHAR_HACK -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

# For Commodore AmigaOS
amiga:
	@echo "Compiling $(EXE) v$(VER) for AmigaDOS (Manx-C 5.2)..."
	$(MAKE) CC=$(CC) "CFLAGS=$(AMIGA_COPTS)" LIBS=-lc EXE=tin linkit

# For Apollo 
apollo:
	@echo "Compiling $(EXE) v$(VER) for Apollo..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

# For BSD-derived systems
bsd:
	@echo "Compiling $(EXE) v$(VER) for BSD/Dec/DG/NeXT/Sun..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

# For Sequent DYNIX
dynix:
	@echo "Compiling $(EXE) v$(VER) for Sequent DYNIX..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap -lseq" \
			 EXE=tin linkit

# For Minix 386
minix:
	@echo "Compiling $(EXE) v$(VER) for Minix 386..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DMINIX -D_POSIX_SOURCE -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses" \
			 EXE=tin linkit
	chmem +131072 $(EXE) 

# For Sequent PTX
#			 NETLIBS="-lsocket -linet -lnsl_s" \
#
ptx:
	@echo "Compiling $(EXE) v$(VER) for Sequent PTX..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DPTX -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

# For SCO Unix
#			 NETLIBS="-lnsl_s -lsocket" \
#
sco:
	@echo "Compiling $(EXE) v$(VER) for SCO Unix..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSYSV -DSCO_UNIX -UM_XENIX -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR="\"$(LIBDIR)\"" -DSPOOLDIR="\"$(SPOOLDIR)\""' \
			 LIBS="-lcurses -lgen -lc_s" \
			 EXE=tin linkit

# For SGI Irix
irix:
	@echo "Compiling $(EXE) v$(VER) for SGI Irix..."
	@$(MAKE) CFLAGS='$(COPTS) -cckr -DSYSV -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lsun -lcurses -ltermcap" \
			 EXE=tin linkit

# For SNI Sinix
sinix:
	@echo "Compiling $(EXE) v$(VER) for SNI Sinix..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DNNTP_INEWS_GATEWAY="\\"$(NNTP_INEWS_GATEWAY)\\" -DNNTP_INEWS_DOMAIN="\\"$(NNTP_INEWS_DOMAIN)\\" -DLIBDIR=\\"$(LIBDIR)\\" -DSPOOLDIR=\\"$(SPOOLDIR)\\"' \
			 LIBS="-lcurses" \
			 EXE=tin linkit

# For System V
#			 NETLIBS="-lnet -lnsl_s" \
#
sysv:
	@echo "Compiling $(EXE) v$(VER) for System V..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSYSV -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

# For System V Release 4
#			 NETLIBS="-lnsl -lsocket" \
#
sysvr4:
	@echo "Compiling $(EXE) v$(VER) for System V Release 4..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSVR4 -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

# For NCR Tower
#			 NETLIBS="-lnet -lnsl_s" \
#
tower:
	@echo "Compiling $(EXE) v$(VER) for NCR Tower..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSYSV -DNCR -DISTRING -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR=\"$(LIBDIR)\" -DSPOOLDIR=\"$(SPOOLDIR)\"' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

# For Xenix 386
#			 NETLIBS="-lsocket" \
#
xenix:
	@echo "Compiling $(EXE) v$(VER) for Xenix 386..."
	@$(MAKE) CC=$(CC) CFLAGS='-c -Zi -DSYSV -DNNTP_INEWS_GATEWAY="\\"$(NNTP_INEWS_GATEWAY)\\" -DNNTP_INEWS_DOMAIN="\\"$(NNTP_INEWS_DOMAIN)\\" -DLIBDIR="\\"$(LIBDIR)\\"" -DSPOOLDIR="\\"$(SPOOLDIR)\\""' \
			 LIBS="-lcurses -ltinfo -lx" \
			 LFLAGS=-Zi EXE=tin linkit

# !!!  THE FOLLOWING ARE SITE SPECIFIC - IGNORE  !!!
ccenter_obj:
	@echo "Compiling a CL target for SUN01 with NNTP ONLY..."
	#setopt program_name $(EXE)
	#setopt sys_load_flags -Dsun -Dsparc -I/usr/include -I. -L/usr/lib -lcurses -ltermcap
	#load -c -g -DBSD -DNNTP_ABLE -DNNTP_ONLY -lcurses -ltermcap $(CFILES)
	#link

#	@$(MAKE) CC=gcc CFLAGS='-c -O2 -Wall -DSYSV -DHAVE_ISPELL -DNNTP_ABLE -DDONT_HAVE_NNTP_EXTS -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
#
anl433:
	@echo "Compiling $(EXE) v$(VER) for ANL433 with NNTP..."
	@$(MAKE) CC=gcc CFLAGS='-c -O2 -Wall -Wshadow -DSYSV -DHAVE_MAIL_HANDLING -DHAVE_ISPELL -DNNTP_ABLE -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap" \
			 NETLIBS="-linet -lnsl_s" \
			 EXE=tin linkit

anl433cd:
	@echo "Compiling $(EXE) v$(VER) for ANL433 with CDROM..."
	@$(MAKE) CC=gcc CFLAGS='-c -O2 -Wall -Wshadow -DSYSV -DCDROM_ABLE -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap $(CDLIB)" \
			 NETLIBS="-linet -lnsl_s" \
			 EXE=cdtin linkit

apollo02:
	@echo "Compiling $(EXE) v$(VER) for APOLLO02 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DNNTP_ONLY -DNNTP_NEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

cox01:
	@echo "Compiling $(EXE) v$(VER) for COX01 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -systype bsd43 -UHAVE_STDLIB_H -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-L/usr/bsd43/usr/lib -lcurses -ltermcap -lc" \
			 EXE=tin linkit

daemon:
	@echo "Compiling $(EXE) v$(VER) for ANL433 with INDEX DAEMON..."
	@$(MAKE) CC=gcc CFLAGS='-c -O2 -DSYSV -DINDEX_DAEMON -DDEBUG' \
			 EXE=$(EXED) linkit

dec14:
	@echo "Compiling $(EXE) v$(VER) for DEC14 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

dg01:
	@echo "Compiling $(EXE) v$(VER) for DG01 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			LIBS="-lcurses -ltermcap" \
			EXE=tin linkit

jupiter:
	@echo "Compiling $(EXE) v$(VER) for JUPITER with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSYSV -DSCO_UNIX -UM_XENIX -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(NNTP_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DLIBDIR="\"$(LIBDIR)\"" -DSPOOLDIR="\"$(SPOOLDIR)\""' \
			 NETLIBS="-lnsl_s -lsocket" \
			 LIBS="-lcurses -lgen -lc_s" \
			 EXE=tin linkit

os2unix:
	@echo "Compiling $(EXE) v$(VER) for OS2UNIX with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSYSV -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap" \
			 NETLIBS="-linet -lnsl_s" \
			 EXE=tin linkit

mx331:
	@echo "Compiling $(EXE) v$(VER) for MX331 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSYSV -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses" \
			 EXE=tin linkit

mx351:
	@echo "Compiling $(EXE) v$(VER) for MX351 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='-c -DSVR4 -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DEBUG' \
			 NETLIBS="-lnsl -lsocket" \
			 LIBS="-lcurses -ltermlib" \
			 EXE=tin linkit

sony01:
	@echo "Compiling $(EXE) v$(VER) for SONY01 with NNTP ONLY..."
	@$(MAKE) CC=cc CFLAGS='-c -g -DBSD -DNNTP_ONLY -DHAVE_MAIL_HANDLING -DHAVE_ISPELL -DLIBDIR=\"/a/usr/lib/news\" -DSPOOLDIR=\"/a/usr/spool/news\" -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			LIBS="-lcurses -ltermcap" \
			EXE=tin linkit

stratus01:
	@echo "Compiling $(EXE) v$(VER) for STRATUS FTX with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DSVR4 -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap" \
			 NETLIBS="-lsocket -lnsl" \
			 EXE=tin linkit

sparc10_1:
	@echo "Compiling $(EXE) v$(VER) for SPARC10 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

sparc10cd:
	@echo "Compiling $(EXE) v$(VER) for SPARC10 with CDROM ABLE..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -DCDROM_ABLE -DDEBUG' \
 			 NNTPLIB="$(CDLIB)" \
			 LIBS="-lcurses -ltermcap" \
			 EXE=cdtin linkit

sun01:
	@echo "Compiling $(EXE) v$(VER) for SUN01 with NNTP ONLY..."
	@$(MAKE) CC=$(CC) CFLAGS='$(COPTS) -DBSD -DNNTP_ONLY -DNNTP_INEWS_GATEWAY=\"$(ANL_INEWS_GATEWAY)\" -DNNTP_INEWS_DOMAIN=\"$(NNTP_INEWS_DOMAIN)\" -DDEBUG' \
			 LIBS="-lcurses -ltermcap" \
			 EXE=tin linkit

linkit: $(OFILES)
	@echo "Linking $(EXE) v$(VER)..."
	$(LD) $(LFLAGS) -o $(EXE) $(OFILES) $(NNTPLIB) $(NETLIBS) $(LIBS) 
	@ls -l $(EXE)

install:
	@echo "Installing $(EXE) v$(VER)..."
	@$(STRIP) $(EXE)
	@-$(STRIP2) $(EXE)
	@-mv $(EXE) $(BINDIR)
	@-rm $(BINDIR)/r$(EXE)
	@ln $(BINDIR)/$(EXE) $(BINDIR)/r$(EXE)
	@chmod 755 $(BINDIR)/$(EXE)
	@ls -l $(BINDIR)/$(EXE) $(BINDIR)/r$(EXE)
	@cp $(EXE).1 $(MANDIR)$(MANEXT)/$(EXE).$(MANEXT)

install_setuid:
	@echo "Installing SETUID $(EXE) v$(VER)..."
	@$(STRIP) $(EXE)
	@-$(STRIP2) $(EXE)
	@-mv $(EXE) $(BINDIR)
	@chown news $(BINDIR)/$(EXE) 
	@chgrp news $(BINDIR)/$(EXE) 
	@-rm $(BINDIR)/r$(EXE)
	@ln $(BINDIR)/$(EXE) $(BINDIR)/r$(EXE)
	@su news -c "chmod 6755 $(BINDIR)/$(EXE)"
	@ls -l $(BINDIR)/$(EXE) $(BINDIR)/r$(EXE)
	@cp $(EXE).1 $(MANDIR)

install_daemon:
	@echo "Installing index daemon $(EXED) v$(VER)..."
	@$(STRIP) $(EXED)
	@-$(STRIP2) $(EXE)
	@-mv $(EXED) $(BINDIR)
	@chown news $(BINDIR)/$(EXED) 
	@chmod 0755 $(BINDIR)/$(EXED)
	@ls -l $(BINDIR)/$(EXED)

proto:
	@echo "Generating function prototypes for proto.h..."
	@echo "#if __STDC__" > PROTO.H
	@echo " " >> PROTO.H
	@cproto -e $(CFILES) >> PROTO.H
	@echo " " >> PROTO.H
	@echo "#else" >> PROTO.H
	@echo " " >> PROTO.H
	@cproto -e -f1 $(CFILES) >> PROTO.H
	@echo " " >> PROTO.H
	@echo "#endif" >> PROTO.H
	@-mv PROTO.H proto.h

nroff:
	@echo "Creating nroff man page..."
	@$(ROFF) $(EXE).1 > $(EXE).nrf

manifest:
	@echo "Creating MANIFEST..."
	@echo "MANIFEST for $(PROJECT)-$(VER) (`date`)" > MANIFEST
	@echo "----------------------------------------------------" >> MANIFEST
	@wc -c $(ALL_FILES) >> MANIFEST

shar:
	@echo "Generating shell archive..."
	@make nroff
	@make manifest
	@make chmod
	@shar -a -F -L60 -n $(PROJECT)-$(VER) -s $(MAIL_ADDR) \
		-o ../$(PROJECT)-$(VER) $(ALL_FILES)

uuencode:
	@make tar
	@echo "Uuencoding $(PROJECT).tar.Z..."
	@uuencode $(PROJECT).tar.Z $(PROJECT).tar.Z > $(PROJECT).tar.Z.uue
	@ls -l $(PROJECT).tar.Z.uue
 
diff:
	@echo "Generating diffs against $(PROJECT)-$(BASE_VER)..."
	@make nroff
	@make manifest
	@make chmod
	@-mv -f $(PROJECT)-$(VER).diff $(PROJECT)-$(VER).diff-
	@-diff -rcs ../$(BASE_VER) . > $(PROJECT)-$(VER).diff
	@ls -l $(PROJECT)-$(VER).diff

patch:
	@make diff
	@echo "Generating patch against $(PROJECT)-$(BASE_VER)..."
	@-mv $(PROJECT)-$(VER).diff $(PROJECT)-$(VER).patch
	@shar -a -n $(PROJECT)-$(VER) -s $(MAIL_ADDR) -L60 \
		-o ../patch-$(VER) $(PROJECT)-$(VER).patch
	@ls -l ../patch-$(VER).*

tar:
	@echo "Generating compressed tar file..."
	@-rm $(PROJECT).tar $(PROJECT).tar.Z > /dev/null 2>&1
	@make chmod
	@tar cvf $(PROJECT).tar $(ALL_FILES)  
	@echo "Compressing $(PROJECT).tar..."
	@compress $(PROJECT).tar 
	@ls -l $(PROJECT).tar.Z

ftptar:
	@echo "Creating FTP tar file with a directory of $(PROJECT)-$(VER)..."
	@mkdir $(PROJECT)-$(VER)
	@chmod 755 $(PROJECT)-$(VER)
	@make chmod
	@cp $(ALL_FILES) $(PROJECT)-$(VER)
	@tar cvf $(PROJECT)-$(VER).tar $(PROJECT)-$(VER)
	@echo "Compressing $(PROJECT)-$(VER).tar..."
	@compress $(PROJECT)-$(VER).tar 
	@ls -l $(PROJECT)-$(VER).tar.Z

chmod:
	@chmod 644 $(ALL_FILES)

zip:
	@echo "Generating zip archive file..."
	@-rm $(PROJECT).zip > /dev/null 2>&1
	@zip $(PROJECT).zip  $(ALL_FILES)
	@ls -l $(PROJECT).zip

zoo:
	@echo "Generating zoo archive file..."
	@-rm $(PROJECT).zoo > /dev/null 2>&1
	@zoo ah $(PROJECT).zoo  $(ALL_FILES)
	@ls -l $(PROJECT).zoo

tags:
	@echo "Generating tags (results in ./tags)..."
	@-rm tags
	@ctags $(HFILES) patchlev.h $(CFILES) 

lint:
	@echo "Linting source (results in ./LINT)..."
	@lint $(LINTFLAGS) -DNNTP_ABLE $(CFILES) > LINT

clean:
	@echo "Cleaning..."
	-/bin/rm -f $(OFILES)

clobber:
	@echo "Clobbering..."
	/bin/rm -f $(OFILES) $(EXE) tags 

cflow:
	@echo "Creating cflow for $(PROJECT)..."
	@cflow $(CFILES) > cflow.$(PROJECT) &

cscope:
	@echo "Creating cscope database $(PROJECT)..."
	@cscope $(ALL_FILES)

man:
	@echo "Printing $(EXE) manual..."
	@$(ROFF) $(EXE).1 | lpr

print:
	for FILE in $(HFILES) patchlev.h $(CFILES) $(SUPPORT); do \
	echo "Printing $$FILE..."; \
	a2ps $$FILE | lpr; \
	done

actived:
	@echo "Compiling actived for AmigaDOS (Manx-C 5.2)..."
	$(CC) CFLAGS=$(AMIGA_COPTS) actived
	$(LD) actived -lc 
	
active.o:	active.c $(HFILES)
amiga.o:	amiga.c $(HFILES)
art.o:		art.c $(HFILES)
curses.o:	curses.c $(HFILES)
debug.o:	debug.c $(HFILES)
envarg.o:	envarg.c $(HFILES)
feed.o:		feed.c $(HFILES)
getline.o:	getline.c $(HFILES)
group.o:	group.c $(HFILES)
hashstr.o:	hashstr.c $(HFILES)
help.o:		help.c $(HFILES)
inews.o:	inews.c $(HFILES)
init.o:		init.c $(HFILES)
kill.o:		kill.c $(HFILES)
lang.o:		lang.c $(HFILES)
mail.o:		mail.c $(HFILES) patchlev.h
main.o:		main.c $(HFILES) patchlev.h
memory.o:	memory.c $(HFILES)
misc.o:		misc.c $(HFILES)
newsrc.o:	newsrc.c $(HFILES)
nntplib.o:	nntplib.c $(HFILES)
open.o:		open.c $(HFILES) patchlev.h
page.o:		page.c $(HFILES)
parsedate.o:	parsedate.y $(HFILES)
post.o:		post.c $(HFILES) patchlev.h
prompt.o:	prompt.c $(HFILES)
rcfile.o:	rcfile.c $(HFILES)
save.o:		save.c $(HFILES)
screen.o:	screen.c $(HFILES)
search.o:	search.c $(HFILES)
select.o:	select.c $(HFILES)
sigfile.o:	sigfile.c $(HFILES)
signal.o:	signal.c $(HFILES)
spooldir.o:	spooldir.c $(HFILES)
strftime.o:	strftime.c $(HFILES)
thread.o:	thread.c $(HFILES)
wildmat.o:	wildmat.c
