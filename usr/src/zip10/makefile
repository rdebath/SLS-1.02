# Makefile for Zip, ZipNote, ZipSplit, and Ship.
# Note: this makefile leaves out the encryption/decryption part of zip, and
#  completely leaves out the zipcloak program.

# what you can make ...
default:
	@echo ''
	@echo 'Make what?  You must say what system to make Zip for--e.g.'
	@echo '"make bsd".  Choices: bsd, bsdold, sysv, sun, next, next10,'
	@echo 'hpux, dnix, cray, 3b1, zilog, aux, convex, aix, and linux.'
	@echo 'See the file zip.doc for more information.'
	@echo ''

# variables (to use the Gnu compiler, change cc to gcc in CC and BIND)
MAKE = make
CC = cc
BIND = cc
RENAME = mv

# flags
#   CFLAGS    flags for C compile
#   LFLAGS1   flags after output file spec, before obj file list
#   LFLAGS2   flags after obj file list (libraries, etc)
CFLAGS = 
LFLAGS1 =
LFLAGS2 =  

# object file lists
OBJZ = zip.o zipfile.o zipup.o fileio.o util.o tempf.o shrink.o globals.o
OBJI = implode.o im_lmat.o im_ctree.o im_bits.o
OBJN = zipnote.o zipfile_.o zipup_.o fileio_.o globals.o
OBJS = zipsplit.o zipfile_.o zipup_.o fileio_.o globals.o

# suffix rules
.SUFFIXES:
.SUFFIXES: _.o .o .c .doc .1
.c_.o:
	$(RENAME) $< $*_.c
	$(CC) $(CFLAGS) -DUTIL -DEXPORT -c $*_.c
	$(RENAME) $*_.c $<
.c.o:
	$(CC) $(CFLAGS) -DEXPORT -c $<
.1.doc:
	nroff -man $< | col -b > $@

# rules for zip, zipnote, zipsplit, and zip.doc.
$(OBJZ): zip.h ziperr.h tempf.h tailor.h
$(OBJI): implode.h crypt.h ziperr.h tempf.h tailor.h
$(OBJN): zip.h ziperr.h tailor.h
$(OBJS): zip.h ziperr.h tailor.h
zip.o zipup.o zipnote.o zipsplit.o: revision.h
zips: zip zipnote zipsplit ship
zipsman: zip zipnote zipsplit ship zip.doc
zip: $(OBJZ) $(OBJI)
	$(BIND) -o zip $(LFLAGS1) $(OBJZ) $(OBJI) $(LFLAGS2)
zipnote: $(OBJN)
	$(BIND) -o zipnote $(LFLAGS1) $(OBJN) $(LFLAGS2)
zipsplit: $(OBJS)
	$(BIND) -o zipsplit $(LFLAGS1) $(OBJS) $(LFLAGS2)
ship: ship.c
	$(CC) $(CFLAGS) -o ship $(LFLAGS1) ship.c $(LFLAGS2)

# These symbols, when #defined using -D have these effects on compilation:
# ZMEM		- includes C language versions of memset(), memcpy(), and
#		  memcmp() (util.c).
# DIRENT	- use <sys/dirent.h> and getdents() instead of <sys/dir.h>
#		  and opendir(), etc. (fileio.c).
# NODIR		- used for 3B1, which has neither getdents() nor opendir().
# NDIR		- use "ndir.h" instead of <sys/dir.h> (fileio.c).
# UTIL		- select routines for utilities (note and split).
# PROTO		- enable function prototypes.
# RMDIR		- remove directories using a system("rmdir ...") call.
# CONVEX	- for Convex make target.
# AIX		- for AIX make target.
# EXPORT	- leave out the encryption code.

# BSD 4.3 (also Unisys 7000--AT&T System V with heavy BSD 4.2)
bsd:
	$(MAKE) zips CFLAGS="-O"

# BSD, but missing memset(), memcmp().
bsdold:
	$(MAKE) zips CFLAGS="-O -DZMEM"

# AT&T System V, Rel 3.  Also SCO, Xenix, OpenDeskTop, ETA-10P*, SGI.
sysv:
	$(MAKE) zips CFLAGS="-O -DDIRENT"

# DNIX 5.x: like System V but optimization is messed up.
dnix:
	$(MAKE) zips CFLAGS="-DDIRENT"

# Sun OS 4.x: BSD, but use getdents().
sun:
	$(MAKE) zips CFLAGS="-O -DDIRENT"

# NeXT 1.0: BSD, but use shared library.
next10:
	$(MAKE) zips CFLAGS="-O" LFLAGS2="-s -lsys_s"

# NeXT 2.0: BSD, but use MH_OBJECT format for smaller executables.
next:
	$(MAKE) zips CFLAGS="-O" LFLAGS2="-s -object"

# HPUX: System V, but use <ndir.h> and opendir(), etc.
hpux:
	$(MAKE) zips CFLAGS="-O -DNDIR"

# Cray Unicos 5.1.10 & 6.0.11, Standard C compiler 2.0
cray:
	$(MAKE) zips CFLAGS="-O -DDIRENT" CC="scc"

# AT&T 3B1: System V, but missing a few things.
3b1:
	$(MAKE) zips CFLAGS="-O -DNODIR -DRMDIR"

# zilog zeus 3.21
zilog:
	$(MAKE) zips CFLAGS="-O -DZMEM -DNDIR -DRMDIR" CC="scc -i"

# SCO 386 cross compile for MS-DOS
# Note: zip.exe should be lzexe'd on DOS to reduce its size
scodos:
	$(MAKE) zips CFLAGS="-O -Ms -dos -DNO_ASM" LFLAGS1="-Ms -dos" \
	 LFLAGS2=""
	$(RENAME) zip zip.exe

# A/UX:
aux:
	$(MAKE) zips CFLAGS="-O -DTERMIO"

# Convex C220, OS 9.0
convex:
	$(MAKE) zips CFLAGS="-O2 -rl -DCONVEX"

# AIX Version 3.1 for RISC System/6000 
aix:
	$(MAKE) zips CC="c89" BIND="c89" \
	   CFLAGS="-O -D_POSIX_SOURCE -D_ALL_SOURCE -D_BSD -DAIX"

# MINIX 1.5.10 with Bruce Evans 386 patches and gcc/GNU make
minix:
	$(MAKE) zips CFLAGS="-g -DDIRENT -DMINIX" CC=gcc BIND=gcc
 	chmem =262144 zip

# Linux 0.96a or above. Use gcc/GNU make.
# We use static libs in order to avoid your problems.
linux:
	$(MAKE) zips CFLAGS="-static -DLINUX" LFLAGS2="-static" CC=gcc BIND=gcc

# clean up after making stuff and installing it
clean:
	rm -f *.o
	rm -f zip zipnote zipsplit ship

# This one's for my own use during development.
it:
	$(MAKE) zipsman CFLAGS="-O -Wall -DPROTO" LFLAGS2="-s -object"\
	VPATH="${HOME}/Unix/bin"

# end of Makefile
