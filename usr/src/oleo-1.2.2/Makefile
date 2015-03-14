# Generated automatically from Makefile.in by configure.
# Copyright (C) 1992, 1993 Free Software Foundation, Inc.
# This file is part of GNU Oleo.
# 
# GNU Oleo is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Oleo is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU Oleo; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
SHELL = /bin/sh

srcdir = .
VPATH = .

# Common prefix for machine-independent installed files.
prefix = /usr
# Common prefix for machine-dependent installed files.
exec_prefix = $(prefix)
# Name under which to install GNU Oleo.
instname = oleo
# Directory to install `oleo' in.
bindir = $(exec_prefix)/bin

CFLAGS = -O6
BISONFLAGS =
optional_libs =  -lX11 -lcurses -ltermcap
extra_obs = 

CC = gcc
BISON = bison -y
INSTALL = /bin/install -c
INSTALL_DATA = $(INSTALL) -m 644
INSTALL_PROGRAM = $(INSTALL)
ETAGS = etags -tw
CTAGS = ctags -tw

ALL_CFLAGS = $(CFLAGS)  -DSTDC_HEADERS=1 -DHAVE_UNISTD_H=1 -DDIRENT=1 -DHAVE_SYS_TIME_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_X11_X_H=1 -DRETSIGTYPE=void -DHAVE_ALLOCA_H=1 -DHAVE_RENAME=1 -DHAVE_GETCAP=1 -DHAVE_STRDUP=1 -DHAVE_STRSTR=1
ALL_BISONFLAGS = $(BISONFLAGS) 

.c.o:
	$(CC) -c $(ALL_CFLAGS) $(CPPFLAGS) -I$(srcdir) $<

libs =  $(optional_libs) -lm 

objs = parse.tab.o byte-compile.o eval.o ref.o \
 decompile.o sort.o regions.o utils.o obstack.o lists.o\
 io-term.o getopt.o getopt1.o io-utils.o io-x11.o window.o io-edit.o\
 hash.o panic.o line.o io-curses.o font.o display.o print.o init.o \
 sylk.o oleofile.o sc.o list.o busi.o string.o cells.o ir.o xrdb.o

all_objs= $(objs) $(extra_obs)

srcs =  parse.y byte-compile.c eval.c ref.c \
 decompile.c sort.c regions.c utils.c obstack.c lists.c\
 io-term.c getopt.h getopt.c getopt1.c  io-utils.c io-x11.c window.c io-edit.c\
 hash.c panic.c line.c  \
 io-curses.c font.c display.c print.c init.c \
 sylk.c oleofile.c sc.c list.c busi.c string.c cells.c \
 random.c vfprintf.c vsprintf.c _doprnt.c alloca.c

headers = byte-compile.h cell.h cmd.h ir.h display.h errors.h eval.h \
 font.h funcdef.h global.h hash.h init.h io-abstract.h io-edit.h \
 io-generic.h io-term.h io-utils.h line.h list.h \
 lists.h  node.h obstack.h oleofile.h parse.h  \
 proto.h ref.h regions.h sc.h sylk.h sysdef.h \
 utils.h window.h ir.c xrdb.c io-curses.h io-x11.h \
 print.h


dist_files = $(srcs) $(headers) parse.tab.c configure Makefile.in \
	configure.in ANNOUNCE COPYING USING BUGS ChangeLog NEWS INSTALL.OLEO \
	README INSTALL

all: oleo
install: all
	$(INSTALL_PROGRAM) oleo $(bindir)/oleo
uninstall:
	rm $(bindir)/oleo
Makefile: $(srcdir)/Makefile.in 
	$(SHELL) config.status
clean:
	rm -f $(all_objs) oleo
distclean: clean
	rm -f Makefile config.status 
mostlyclean: clean
realclean: distclean
	rm -f  parse.tab.c
TAGS: $(srcs)
	$(ETAGS) $(srcs)
tags: $(srcs)
	$(CTAGS) $(srcs)
dist.afs: $(dist_files)
	echo oleo-`sed -e '/version_string/!d' -e 's/[^0-9.]*\([0-9.]*\).*/\1/' -e q io-term.c` > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	cd `cat .fname`; \
	for file in $(dist_files); do ln -s ../"$$file" .; done; \
	cd ..
	tar chZf `cat .fname`.tar.Z `cat .fname`
	rm -rf `cat .fname` .fname

dist: $(dist_files)
	echo oleo-`sed -e '/version_string/!d' -e 's/[^0-9.]*\([0-9.]*\).*/\1/' -e q io-term.c` > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln $(dist_files) `cat .fname`
	tar chZf `cat .fname`.tar.Z `cat .fname`
	rm -rf `cat .fname` .fname

distfiles:
	echo $(dist_files)

check:
	echo check check check
oleo: $(all_objs)
	$(CC) $(ALL_CFLAGS) -o oleo $(all_objs) $(libs)

parse.tab.c: parse.y
	$(BISON) parse.y
	-mv y.tab.c parse.tab.c

# Prevent GNU make v3 from overflowing arg limit on SysV.
.NOEXPORT:

# ================ automaticly generated ================

_doprnt.o : _doprnt.c 
alloca.o : alloca.c 
busi.o : busi.c funcdef.h sysdef.h global.h utils.h cell.h font.h line.h eval.h \
  errors.h 
byte-compile.o : byte-compile.c funcdef.h obstack.h sysdef.h global.h utils.h \
  node.h eval.h cell.h font.h line.h hash.h ref.h 
cells.o : cells.c obstack.h funcdef.h sysdef.h global.h utils.h cell.h font.h \
  line.h eval.h errors.h lists.h 
decompile.o : decompile.c funcdef.h sysdef.h global.h utils.h eval.h cell.h \
  font.h line.h io-utils.h 
display.o : display.c display.h global.h sysdef.h utils.h funcdef.h font.h line.h \
  cell.h ir.h proto.h lists.h io-utils.h 
eval.o : eval.c funcdef.h obstack.h sysdef.h global.h utils.h cell.h font.h \
  line.h eval.h errors.h 
font.o : font.c font.h global.h sysdef.h utils.h funcdef.h line.h window.h cell.h \
  io-abstract.h 
getopt.o : getopt.c getopt.h 
getopt1.o : getopt1.c getopt.h 
hash.o : hash.c hash.h 
init.o : init.c proto.h init.h global.h sysdef.h utils.h funcdef.h io-term.h 
io-curses.o : io-curses.c proto.h funcdef.h sysdef.h global.h utils.h cell.h \
  font.h line.h cmd.h io-generic.h obstack.h io-edit.h io-term.h io-abstract.h \
  window.h io-utils.h lists.h 
io-edit.o : io-edit.c funcdef.h sysdef.h global.h utils.h cell.h font.h line.h \
  io-edit.h cmd.h 
io-term.o : io-term.c getopt.h funcdef.h obstack.h sysdef.h global.h utils.h \
  cell.h font.h line.h cmd.h init.h regions.h ref.h lists.h io-abstract.h window.h \
  io-generic.h io-utils.h io-edit.h io-term.h io-x11.h io-curses.h sylk.h print.h \
   oleofile.h sc.h list.h 
io-utils.o : io-utils.c sysdef.h io-utils.h global.h utils.h funcdef.h cell.h \
  font.h line.h io-generic.h obstack.h io-abstract.h window.h lists.h 
io-x11.o : io-x11.c 
ir.o : ir.c ir.h proto.h 
line.o : line.c global.h sysdef.h utils.h funcdef.h line.h 
list.o : list.c funcdef.h sysdef.h global.h utils.h cell.h font.h line.h io-generic.h \
  obstack.h io-abstract.h window.h regions.h io-utils.h 
lists.o : lists.c funcdef.h obstack.h sysdef.h global.h utils.h cell.h font.h \
  line.h ref.h window.h 
obstack.o : obstack.c obstack.h 
oleofile.o : oleofile.c funcdef.h sysdef.h io-generic.h global.h utils.h obstack.h \
  io-abstract.h line.h window.h cell.h font.h io-utils.h io-term.h sylk.h lists.h \
  ref.h 
panic.o : panic.c funcdef.h sysdef.h global.h utils.h io-generic.h obstack.h \
  io-abstract.h line.h window.h cell.h font.h 
parse.tab.o : parse.tab.c funcdef.h obstack.h sysdef.h global.h utils.h errors.h \
  node.h eval.h cell.h font.h line.h ref.h 
print.o : print.c display.h global.h sysdef.h utils.h funcdef.h font.h line.h \
  cell.h ir.h proto.h 
random.o : random.c 
ref.o : ref.c funcdef.h sysdef.h global.h utils.h cell.h font.h line.h eval.h \
  io-abstract.h window.h hash.h byte-compile.h parse.h ref.h 
regions.o : regions.c funcdef.h sysdef.h global.h utils.h cell.h font.h line.h \
  io-generic.h obstack.h io-abstract.h window.h io-utils.h lists.h ref.h regions.h \
  io-term.h 
sc.o : sc.c funcdef.h sysdef.h global.h utils.h cell.h font.h line.h io-generic.h \
  obstack.h io-abstract.h window.h io-utils.h lists.h ref.h parse.h regions.h 
sort.o : sort.c 
string.o : string.c funcdef.h obstack.h sysdef.h global.h utils.h cell.h font.h \
  line.h eval.h errors.h 
sylk.o : sylk.c funcdef.h sysdef.h io-generic.h global.h utils.h obstack.h io-abstract.h \
  line.h window.h cell.h font.h io-term.h lists.h io-utils.h ref.h regions.h 
utils.o : utils.c sysdef.h utils.h funcdef.h global.h 
vfprintf.o : vfprintf.c sysdef.h 
vsprintf.o : vsprintf.c sysdef.h 
window.o : window.c global.h sysdef.h utils.h funcdef.h window.h cell.h font.h \
  line.h io-generic.h obstack.h io-abstract.h io-utils.h io-term.h cmd.h lists.h 
xrdb.o : xrdb.c 
