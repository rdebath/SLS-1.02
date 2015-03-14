# Makefile for GNU binary-file utilities
# Select a set of CFLAGS and PROGS, below, depending on the system type.
# Copyright (C) 1989, 1990 Free Software Foundation, Inc.
#
# This file is part of the GNU binutils.
# 
# The GNU binutils are free software; you can redistribute them and/or modify
# them under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
# 
# The GNU binutils are distributed in the hope that they will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with the GNU binutils; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# gprof.c contains `void *' and very long string constants which many
# C compilers can't handle, so you might need gcc to compile it.
GNUCC = gcc -O
# But you might need to use your old C compiler to compile the other
# programs (ld at least), if you can't run gcc without them
# (particularly on COFF systems).
#CC = cc

bindir=/usr/local/gnubin

# For BSD:
# For SunOS 4.x, take out -DVPRINTF_MISSING.
CFLAGS = -g -DVPRINTF_MISSING
# Don't add robotussin; it won't compile on BSD or GNU systems.
# objdump is not here because it (at least used to) not compile
# on most systems (trouble with N_DATADDR).  Some of those problems
# have been fixed, though.
PROGS = $(archpfx)gprof $(archpfx)ld $(archpfx)size \
        $(archpfx)nm $(archpfx)strip $(archpfx)ar $(archpfx)ranlib

# For USG systems using COFF_ENCAPSULATE:
# Also, you will want to make the target libc.a (but it takes a long time).
# Note that you should leave a copy of `ar' in this directory
# after you install it, since `ranlib' will try to run it from here.
#CFLAGS=-g -DUSG -DCOFF_ENCAPSULATE -DPORTAR -DNON_NATIVE -DSYS_SIGLIST_MISSING
#PROGS = ld size nm strip ar robotussin objdump ranlib gprof
#SIGNAME = $(archpfx)signame.o
# On ALTOS systems, add -DALTOS to CFLAGS.

#It's better to move a copy of alloca into your libc than to risk getting some
#incompatible functions from -lPW (like index()), but if you
#want to be lazy, uncomment this line.
#ALLOCALIBS = -lPW

# For HP-UX:
# Don't add `robotussin'; use `hpxt' instead.
# Note that you should leave a copy of `ar' in this directory
# after you install it, since `ranlib' will try to run it from here.
#CFLAGS = -g -Ihp-include -DUSG -DNON_NATIVE -DVPRINTF_MISSING
#PROGS = ld size nm strip ar ranlib
#ALLOCALIBS = alloca.o

# For the NeXT:
# Set GNUCC = cc -O.
# Put -DMACH_O in CFLAGS, take out -DVPRINTF_MISSING.
# Comment out MALLOC below to use the system's malloc().

# For Sun386:
# Compile with -DPORTAR -DCOFF_ENCAPSULATE.
# Depending on how you configure gcc, you might also want -Dnounderscore,
# though I did not wind up using it.

# If you run out of stack space while running GNU ar or GNU ld (this
# manifests itself as a segment violation), you should link in alloca.c
# from the gcc sources, and get rid of the "#define alloca" in ar.c and
# ld.c; or you could try to get Sun to fix this annoying "feature".
#CFLAGS = -g -DCOFF_ENCAPSULATE -DPORTAR
#PROGS = ld size nm strip ar robotussin objdump ranlib gprof

# nm tries to malloc enough space for the string table.  The old GNU malloc
# rounds this up to a power of two (e.g. 5M becomes 8M), and so it might 
# fail unnecessarily.  I've also seen some Unix malloc's fail, even when
# there is enough memory.  So use the new GNU malloc.
MALLOC = $(archpfx)gmalloc.o

GNU_GETOPT = $(archpfx)getopt.o
GNU_GETOPT_LONG = $(archpfx)getopt.o $(archpfx)getopt1.o

# C++ demangler
CPLUS_DEM = $(archpfx)cplus-dem.o

LIBS=$(ALLOCALIBS) $(SIGNAME)

all: $(PROGS)

$(archpfx)ld: $(archpfx)ld.o $(GNU_GETOPT_LONG) $(CPLUS_DEM) $(LIBS)
# LIBS is used here since ld needs to use alloca.
# Alternatively, compile it with GNU C--then the compiler handles alloca.
	$(CC) $(CFLAGS) -o $(archpfx)ld $(archpfx)ld.o $(GNU_GETOPT_LONG) $(CPLUS_DEM) $(LIBS)

$(archpfx)size: $(archpfx)size.o $(LIBS)
	$(CC) $(CFLAGS) -o $(archpfx)size $(archpfx)size.o

$(archpfx)nm: $(archpfx)nm.o $(GNU_GETOPT_LONG) $(CPLUS_DEM) $(MALLOC) $(LIBS)
	$(CC) $(CFLAGS) -o $(archpfx)nm $(archpfx)nm.o \
		$(GNU_GETOPT_LONG) $(CPLUS_DEM) $(LIBS) $(MALLOC)

$(archpfx)strip: $(archpfx)strip.o $(archpfx)error.o $(GNU_GETOPT_LONG) $(LIBS)
	$(CC) $(CFLAGS) -o $(archpfx)strip $(archpfx)strip.o $(archpfx)error.o $(GNU_GETOPT_LONG) $(LIBS)

$(archpfx)ar: $(archpfx)ar.o $(LIBS)
	$(CC) $(CFLAGS) -o $(archpfx)ar $(archpfx)ar.o $(LIBS)

$(archpfx)gprof: $(archpfx)gprof.o $(GNU_GETOPT_LONG) $(CPLUS_DEM) $(LIBS)
	$(GNUCC) $(CFLAGS) -o $(archpfx)gprof $(archpfx)gprof.o \
		$(GNU_GETOPT_LONG) $(CPLUS_DEM) $(LIBS)
$(archpfx)gprof.o: gprof.c gmon.h
	$(GNUCC) $(CFLAGS) -c gprof.c $(OUTPUT_OPTION)

$(archpfx)ranlib: $(archpfx)ranlib.o $(GNU_GETOPT_LONG) $(LIBS)
	$(CC) $(CFLAGS) -o $(archpfx)ranlib $(archpfx)ranlib.o $(GNU_GETOPT_LONG) $(LIBS)
$(archpfx)ranlib.o: ranlib.c
	$(CC) -c $(CFLAGS) -DAR_PROG=\"$(bindir)/ar\" ranlib.c $(OUTPUT_OPTION)

$(archpfx)objdump: $(archpfx)objdump.o $(archpfx)error.o $(GNU_GETOPT_LONG) a.out.gnu.h $(LIBS)
	$(CC) $(CFLAGS) -o $(archpfx)objdump $(archpfx)objdump.o $(archpfx)error.o \
		$(GNU_GETOPT_LONG) $(LIBS)

# Robotussin is NOT part of `all'.
$(archpfx)robotussin: $(archpfx)robotussin.o $(LIBS)
	$(CC) $(CFLAGS) -o $(archpfx)robotussin $(archpfx)robotussin.o

libc.a: $(archpfx)robotussin libconvert $(archpfx)ar
	libconvert /lib/libc.a libc.a

# usg-gnulib is the file gcc makes using the usg compiler
gnulib: $(archpfx)robotussin libconvert usg-gnulib $(archpfx)ar
	libconvert usg-gnulib gnulib

clean:
	-rm -f *.o core
	-rm -f $(archpfx)*.o
	-rm -f $(PROGS) objdump

DISTNAME = binutils-1.9
dist:
	rm -rf $(DISTNAME)
	mkdir $(DISTNAME) $(DISTNAME)/hp-bin $(DISTNAME)/hp-include
	ln COPYING ChangeLog Makefile README README-ENCAP \
	a.out.encap.h a.out.gnu.h ar.c cplus-dem.c \
	error.c getopt.c getopt.h getopt1.c gmalloc.c \
	gmon.h gprof.c gprof.texinfo ld.c libconvert nm.c objdump.c \
	ranlib.c ranlib.h robotussin.c signame.c signame.h size.c \
	stab.def stab.h strip.c symseg.h $(DISTNAME)
	cd hp-bin; ln Makefile chatr.c hpxt.c ioutil.c ioutil.h \
	mkhplib ../$(DISTNAME)/hp-bin
	cd hp-include; ln a.out.h stab.def stab.h ../$(DISTNAME)/hp-include
	tar chZf $(DISTNAME).tar.Z $(DISTNAME)
	rm -rf $(DISTNAME)
.PHONY: dist

.PHONY: install
install: $(PROGS)
	for file in $(PROGS); do \
	cp $$file $(bindir)/$${file}.new; \
	mv $(bindir)/$${file}.new $(bindir)/$$file; \
	done
