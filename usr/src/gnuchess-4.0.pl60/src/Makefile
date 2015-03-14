#
# Makefile for GNU Chess
#
# Copyright (c) 1992 Free Software Foundation
#
# This file is part of GNU CHESS.
#
# GNU Chess is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Chess is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Chess; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#

#
# gnuchess  is a curses-based chess.
# gnuchessn is a fancy-display-curses-based chess.
# gnuchessr is a plain dumb-terminal chess (but with full variation output)
# gnuchessc is suitable for chesstool use (mimics /usr/games/chess output)
# gnuchessx is the xchess based chess.
#

# The version number of this GNU and Xboard release
VERS=	4.0.pl60
XVERS = 2.1.pl7

# Relevant file areas.
DIST=	../README ../README.lang ../doc ../misc ../src ../test

# Distribution directory
#DISTDIR=/home/fsf/cracraft/Chess
DISTDIR=/usr/src/usr.bin/gnuchess-4.0.pl60

# Programs being distributed
PROGS=gnuchess-$(VERS) xboard-$(XVERS)
#PROGS=gnuchess-$(VERS)

# For -DGDBM option
LIBS = 

# Change these to something less transitory, like /usr/games, and then
# compile. Ask your system admin / unix guru to put gnuchess.{hash,lang,book}
# in $(LIBDIR).
# Where the binaries live.
#BINDIR= /tmp
BINDIR=/usr/bin

# Where language description, our book, and the persistent hash live.
LIBDIR= /usr/lib

# Display routines.
LCURSES=-lcurses -ltermcap

#compile options for gnuchess
# -DAG[0-4]  selects a set of values for SpaceBonus tables
# -DQUIETBACKGROUND don't print post information in background ( easy OFF)
# -DNOMEMSET if your machine does not support memset
# -DNOMATERIAL don't call it a draw when no pawns and both sides < rook
# -DNODYNALPHA don't dynamically adjust alpha
# -DHISTORY use history killer hueristic 
# -DKILLT use killt killer hueristic 
# -DHASGETTIMEOFDAY use gettimeofday for more accurate timing
# -DCLIENT create client version for use with ICS
# -DNOSCORESPACE don't use Scorespace heuristic
# -DOLDXBOARD don't generate underpromote moves
# -DGNU3 don't generate underpromote moves
# -DLONG64 if you have 64bit longs
# -DSYSV   if you are using SYSV
# -DCACHE  Cache static evaluations 
# -DE4OPENING always open e4 if white and respond to e4 with e5 if black
# -DQUIETBOOKGEN Don't print errors while loading a book or generating a binbook
# -DSEMIQUIETBOOKGEN Print less verbose errors while reading book or generating binbook
# -DGDBM use gdbm based book
# -DGDX  use random file based book
# -DNULLMOVE include null move heuristic
# some debug options
# -DDEBUG8 dump board,movelist,input move to /tmp/DEBUG if illegal move
# -DDEBUG9 dump move list from test command
# -DDEBUG10 dump board and move after search before !easy begins
# -DDEBUG11 dump board when the move is output
# -DDEBUG12 dump boards between moves
# -DDEBUG13 dump search control information for each move to /tmp/DEBUG
# -DDEBUG40 include extra values of variables for debugging  in game list
# -DDEBUG41 dump post output to /tmp/DEBUG
# the rest of the debug options are tied to the debuglevel command
# -DDEBUG -DDEBUG4 set up code for debuglevel command
#          debuglevel
#               1 always force evaluation in evaluate
#               4 print move list after search
#               8 print move list after book before search
#              16 print move list after each ply of search
#              32 print adds to transposition table
#              64 print returns from transposition table lookups
#	      256 print search tree as it is generated
#	      512 debug trace
#	     1024 interactive tree print
#			debug? p#  where # is no. of plys to print from top of tree (default all plys)
#			       XXXX moves specifying branch of tree to print (default all branches)
#			       return terminates input
#	     2048 non-interactive trace print

# ICS client
#OPT= -DAG4 -DQUIETBACKGROUND -DNOSCORESPACE -DHASGETTIMEOFDAY -DGNU3 -DHISTORY -DCACHE -DCLIENT -DE4OPENING -DGDX -DSEMIQUIETBOOKGEN -DNULLMOVE
# gnufour
#OPT= -DAG4 -DNOSCORESPACE -DHASGETTIMEOFDAY -DGNU3 -DHISTORY -DSEMIQUIETBOOKGEN -DQUIETBACKGROUND -DGDX  -DNULLMOVE -DCACHE -DCLIENT
# normal
OPT= -DAG4 -DQUIETBACKGROUND -DNOSCORESPACE -DHASGETTIMEOFDAY -DGNU3 -DHISTORY -DCACHE -DGDX -DSEMIQUIETBOOKGEN -DNULLMOVE

# The hashfile is a record of positions seen. It is used by
# GNU Chess to avoid making the same mistakes, a form of learning.
HASH=	-DHASHFILE=\"$(LIBDIR)/gnuchess.hash\"

# The "book" is a record of the first few moves, for playing good
# moves easily and quickly, saving time, and irritating the human
# opponent.
#BOOK=	-DBOOK=\"$(LIBDIR)/gnuchess.book\"
BOOK=	
BINBOOK = -DBINBOOK=\"$(LIBDIR)/gnuchess.book.data\"

# The language file describes capabilities of the program. Perhaps
# it is useful for non-English speaking countries and customizing
# for their convenience and chess happiness.
LANG= -DLANGFILE=\"$(LIBDIR)/gnuchess.lang\"

# The compiler used for compiling this software.
# Use this for a plain C compiler 
#CC= cc $(OPT)
# Use this for DEC's ANSI C compiler on Ultrix
#CC= c89 $(OPT)
# Use this if you are lucky enough to have GNU CC.
CC=	gcc -W  $(OPT)
#CC=	/contrib/system/bin/gcc -W $(OPT)

# Miscellaneous CFLAGS. Uncomment the one you need and comment 
# the other.
#CFLAGS= -p -Dinline=""	 -traditional-cpp
#CFLAGS= -O4 -Qpath .  # SunOS cc using unprotoize
#CFLAGS= -O4 # Sun acc
#CFLAGS= -g -traditional-cpp  # debug
#CFLAGS= -O2 # DEC ANSI C (c89) on Ultrix.
#CFLAGS= +O3 -Aa -D_HPUX_SOURCE -DSYSV # HPUX cc 
#CFLAGS= -O   -finline-functions -fstrength-reduce -D__mips -D__LANGUAGE_C # gnu cc 1.40 on DS5000
#CFLAGS= -O   -finline-functions -fstrength-reduce  # gnu cc 1.40 on others
#CFLAGS= -O2 -funroll-loops -D__mips -D__LANGUAGE_C # gnu cc 2.00 on DS5000
#CFLAGS= -O2 -D__alpha -D__LANGUAGE_C # gnu cc 2.00 on Flamingo
#CFLAGS= -O2 -funroll-loops -traditional-cpp  # gnu cc  2.00 on SunOS
CFLAGS= -O2 -funroll-loops  # gnu cc  2.00 on others

all : gnuchess gnuchessr gnuchessn gnuchessx gnuchessc postprint gnuan game bincheckr checkgame

gnuchess: mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o dspcomN.o uxdsp.o
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -o gnuchess mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o dspcomN.o uxdsp.o $(LCURSES) $(LIBS)

gnuan: mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o gnuan.o
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -o gnuan mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o gnuan.o $(LIBS)

gnuchessc: mainC.o bookC.o genmovesC.o ataks.o util.o evalN.o init.o searchC.o dspcomC.o nondspC.o
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -o gnuchessc mainC.o bookC.o genmovesC.o ataks.o util.o evalN.o init.o searchC.o dspcomC.o nondspC.o $(LIBS)

Dgnuchessr: mainDR.o bookN.o genmovesN.o ataks.o utilDR.o evalDR.o init.o searchDR.o dspcomDR.o nondspDR.o
	$(CC)  -DDEBUG  -DDEBUG4 $(CFLAGS) $(HASH) $(LANG) $(BOOK) -o gnuchessr mainDR.o bookN.o genmovesN.o ataks.o utilDR.o evalDR.o init.o searchDR.o dspcomDR.o nondspDR.o $(LIBS) $(LIBS)

gnuchessx: mainX.o bookX.o genmovesX.o ataks.o util.o evalN.o init.o searchX.o dspcomX.o nondspX.o
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -o gnuchessx mainX.o bookX.o genmovesX.o ataks.o util.o evalN.o init.o searchX.o dspcomX.o nondspX.o $(LIBS)

gnuchessr: mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o dspcomR.o nondspR.o
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -o gnuchessr mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o dspcomR.o nondspR.o $(LIBS)

gnuchessn: mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o dspcomN.o nuxdsp.o
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -o gnuchessn mainN.o bookN.o genmovesN.o ataks.o util.o evalN.o init.o searchN.o dspcomN.o nuxdsp.o $(LCURSES) $(LIBS)

game: game.c gnuchess.h
	$(CC) $(CFLAGS) -o game game.c
	
postprint: postprint.o
	$(CC) $(CFLAGS) $(HASH) -o postprint postprint.o
	
bincheckr: bincheckr.o
	$(CC) $(CFLAGS) -o bincheckr bincheckr.o
	
checkgame: checkgame.o
	$(CC) $(CFLAGS) -o checkgame checkgame.o

gnuan.o: gnuan.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c gnuan.c

mainN.o: main.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) $(BINBOOK) -c main.c
	mv main.o mainN.o
mainC.o: main.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) $(BINBOOK) -DNONDSP -DCHESSTOOL \
		-c main.c
	mv main.o mainC.o
mainX.o: main.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) $(BINBOOK) -DXBOARD  -c main.c
	mv main.o mainX.o
mainDR.o: main.c gnuchess.h version.h
	$(CC)  -DDEBUG -DDEBUG4 $(CFLAGS) $(HASH) $(LANG) $(BOOK) $(BINBOOK) \
		-c main.c
	mv main.o mainDR.o

genmovesN.o: genmoves.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c genmoves.c
	mv genmoves.o genmovesN.o
genmovesC.o: genmoves.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP -DCHESSTOOL \
		-c genmoves.c 
	mv genmoves.o genmovesC.o
genmovesX.o: genmoves.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DXBOARD \
		-c genmoves.c
	mv genmoves.o  genmovesX.o

bookN.o: book.c gnuchess.h version.h ataks.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) $(BINBOOK) -c book.c 
	mv book.o bookN.o
bookC.o: book.c gnuchess.h version.h ataks.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP -DCHESSTOOL \
	      $(BINBOOK) \
		-c book.c 
	mv book.o bookC.o
bookX.o: book.c gnuchess.h version.h ataks.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DXBOARD  $(BINBOOK) \
		-c book.c
	mv book.o  bookX.o

ataks.o: ataks.h ataks.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c ataks.c

util.o: util.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c util.c

evalN.o: eval.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c eval.c
	mv eval.o evalN.o
evalDR.o: eval.c gnuchess.h version.h
	$(CC)  -DDEBUG4 -DDEBUG $(CFLAGS) $(HASH) $(LANG) $(BOOK) \
		-c eval.c 
	mv eval.o evalDR.o
utilDR.o: util.c gnuchess.h version.h
	$(CC)  -DDEBUG4 -DDEBUG $(CFLAGS) $(HASH) $(LANG) $(BOOK) \
		-c util.c 
	mv util.o utilDR.o

init.o: init.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c init.c

searchN.o: search.c gnuchess.h version.h debug512.h debug10.h debug13.h debug16.h debug256.h debug4.h debug40.h debug41.h debug64.h debug8.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c search.c
	mv search.o searchN.o
searchC.o: search.c gnuchess.h version.h debug512.h debug10.h debug13.h debug16.h debug256.h debug4.h debug40.h debug41.h debug64.h debug8.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP -DCHESSTOOL \
		-c search.c 
	mv search.o searchC.o
searchX.o: search.c gnuchess.h version.h debug512.h debug10.h debug13.h debug16.h debug256.h debug4.h debug40.h debug41.h debug64.h debug8.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DXBOARD \
		-c search.c 
	mv search.o searchX.o
searchDR.o: search.c gnuchess.h version.h debug512.h debug10.h debug13.h debug16.h debug256.h debug4.h debug40.h debug41.h debug64.h debug8.h
	$(CC)  -DDEBUG4 -DDEBUG $(CFLAGS) $(HASH) $(LANG) $(BOOK) \
		-c search.c 
	mv search.o searchDR.o

uxdsp.o: uxdsp.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c uxdsp.c

nuxdsp.o: nuxdsp.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c nuxdsp.c

nondspC.o: nondsp.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP -DCHESSTOOL \
		-c nondsp.c 
	mv nondsp.o nondspC.o
nondspX.o: nondsp.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP -DXBOARD \
		-c nondsp.c 
	mv nondsp.o nondspX.o
nondspR.o: nondsp.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP \
		-c nondsp.c 
	mv nondsp.o nondspR.o
nondspDR.o: nondsp.c gnuchess.h version.h
	$(CC)  -DDEBUG4 -DDEBUG $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP \
		 -c nondsp.c 
	mv nondsp.o nondspDR.o

dspcomN.o: dspcom.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -c dspcom.c
	mv dspcom.o dspcomN.o
dspcomC.o: dspcom.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP -DCHESSTOOL \
		-c dspcom.c 
	mv dspcom.o dspcomC.o
dspcomX.o: dspcom.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP -DXBOARD \
		-c dspcom.c 
	mv dspcom.o dspcomX.o
dspcomR.o: dspcom.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP \
		-c dspcom.c 
	mv dspcom.o dspcomR.o
dspcomDR.o: dspcom.c gnuchess.h version.h
	$(CC)  -DDEBUG -DDEBUG4 $(CFLAGS) $(HASH) $(LANG) $(BOOK) -DNONDSP \
		-c dspcom.c 
	mv dspcom.o dspcomDR.o

postprint.o: postprint.c gnuchess.h version.h
	$(CC) $(CFLAGS) $(HASH) -c postprint.c

distribution:
	-patchlevel=`cat $(DISTDIR)/gnuchess-$(VERS)/src/version.h|grep patchlevel|sed -e 's/[^0-9]//g'` ;\
	echo "GNU patchlevel is $$patchlevel" ;\
	xpatchlevel=`cat $(DISTDIR)/xboard-$(XVERS)/patchlevel.h|sed -e "s/#define PATCHLEVEL //"` ;\
	cd $(DISTDIR) ;\
	rm -f gnuchess.tar.$(VERS).Z* gnuchess.tar.$(VERS).Z.uu* ;\
	tar cf - $(PROGS) | compress > $(DISTDIR)/gnuchess-$(VERS).pl$$patchlevel.tar.Z ;\
	uuencode gnuchess-$(VERS).pl$$patchlevel.tar.Z gnuchess-$(VERS).pl$$patchlevel.tar.Z > gnuchess-$(VERS).pl$$patchlevel.tar.Z.uu ;\
	rm -f x?? ;\
	split -2500 gnuchess-$(VERS).pl$$patchlevel.tar.Z.uu ;\
	for i in x??; do \
	  mv $$i $(DISTDIR)/GNU_Chess_$$i; \
	done

install:
	-cp gnuchessx $(BINDIR)/gnuchessx
	-cp gnuchessc $(BINDIR)/gnuchessc
	-cp gnuchessr $(BINDIR)/gnuchessr
	-cp gnuchessn $(BINDIR)/gnuchessn
	-cp postprint $(BINDIR)/postprint
	-cp gnuan $(BINDIR)/gnuan
	-cp gnuchess $(BINDIR)/gnuchess
	-cp bincheckr $(BINDIR)/bincheckr
	-cp checkgame $(BINDIR)/checkgame
	-cp ../misc/gnuchess.book $(LIBDIR)/gnuchess.book
	-cp ../misc/gnuchess.lang $(LIBDIR)/gnuchess.lang

clean:
	-rm -f gnuchessx gnuchessc gnuchess gnuchessr gnuchessn gnuchessd postprint gnuan bincheckr checkgame game
	-echo $(DISTDIR)
	-rm -f $(DISTDIR)/gnuchess-4.0/misc/gnuchess.book.data
	-find $(DISTDIR) \( -name '*.o' -o -name '*~' -o -name 'CL*' -o -name 'PATCH*' -o -name '#*#' -o -name '%*%' -o -name '*orig' \) -exec rm -f {} \;
