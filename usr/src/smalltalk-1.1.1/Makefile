######################################################################
#
# Copyright (C) 1990, 1991 Free Software Foundation, Inc.
# Written by Steve Byrne, with lots of good input from Dave Ciemiewicz.
#
# This file is part of GNU Smalltalk.
#
# GNU Smalltalk is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 1, or (at your option) any later version.
# 
# GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
# Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
#
#######################################################################

##################
##################
##################
###
### See mst.texinfo for details on installation.
###
##################
##################
##################

##############
#
# These variables you may wish to change
#
##############

# Mail path to GNU Smalltalk maintainer (Steve Byrne)
MAINTAINER = 'sbb@eng.sun.com'

# Uncomment this to use the winning GNU C compiler
#CC = gcc

# These two definitions are used to allow you to add your own files
# and libraries that will be linked into the GNU Smalltalk executable image.
# see ./examples/Makefile for an example of how to use these
USER_OBJS =
USER_LIBS =

# This controls what file gets created on a 'make tar'...it could be the name
# of a tape drive.
TAR_FILE = mst.tar	# change this or override to be correct for your site

# Note: an alternative is  CPP = /lib/cpp
CPP = cc -E

# Uncomment this line to enable the ultra-winning readline-reader
#READLINE = -DUSE_READLINE

######################################################################
######################################################################
######################################################################
###
### You typically won't want to change any variables below this 
### point, although you may want to add rules for your own C functions
### below.
###
######################################################################
######################################################################
######################################################################

MAKE = make  # BSD doesn't have it as a default.

# This controls the distribution version (not completely yet)
VERSION=1.1.1

all: xmakefile $(USER_OBJS)
	@$(MAKE) $(MFLAGS) \
		CC=$(CC) \
		"USER_OBJS=$(USER_OBJS)" "USER_LIBS=$(USER_LIBS)" \
		-f xmakefile mst

mst lexdebug: xmakefile $(USER_OBJS)
	@$(MAKE) $(MFLAGS) CC=$(CC) \
		"USER_OBJS=$(USER_OBJS)" "USER_LIBS=$(USER_LIBS)" \
		-f xmakefile $@



profile:
	$(MAKE) CFLAGS='$(CFLAGS) -pg -Dprofiling' -f xmakefile $@

regress:
	(cd ./test; run-regression-tests)

clean:
	-rm -f *.o
	-rm -f mst mst.im
	-rm -f junk.c xmakefile

#
# Install into the directories pointed to by mstpaths.h
#

install: xmakefile
	@$(MAKE) $(MFLAGS) -f xmakefile install

#
# Create a tar file based on the Smalltalk source files
#
tar:
	rm -f smalltalk-$(VERSION)
	ln -s . smalltalk-$(VERSION)
	sed -e "s/^/smalltalk-$(VERSION)\//" < mstfiles > mstdistfiles
	tar cvf $(TAR_FILE) `cat mstdistfiles`
	rm -f mstdistfiles

xmakefile: ymakefile mstconfig.h mstpaths.h
	@echo "===== creating configuration dependent xmakefile"
	rm -f xmakefile
	cp ymakefile junk.c
	$(CPP) -DFOR_MAKE $(READLINE) junk.c |				\
	sed -e 's/^#.*//' -e 's/^[ \f\t][ \f\t]*$$//' -e 's/^ / /' |	\
	sed -n -e '/^..*$$/p' > xmakefile
	rm -f junk.c
	@echo "===== done creating xmakefile"



#
# Make the shar file for GNU Smalltalk
#
shar:
	shar -imstfiles > mst.shar


#
# (Re)build the tags for Emacs
#
tags:
	etags mst*.[ch]

unfin:
	egrep -n '### ' *.[chy]

optionals:
	egrep -n '??? ' *.[chy]


senddiffs: diffs
	kermit -e 1000 -s mst.diffs

mail-diffs:
	@echo "Please enter a brief note describing the contents of"
	@echo "the diffs file that you've created.  End with Ctrl-D."
	@cat > mst.diffs.note
	@echo "-----------------------------------" >> mst.diffs.note
	@### If there's a more portable/better way to do this, please
	@### don't hesitate to fix this and send the new version back -- SBB
	@cat mst.diffs.note mst.diffs | mail $(MAINTAINER)

diffs:
	mk_mst_diffs

setup:
	@if [ -f ./orig ] ; then \
		echo "./orig exists, but is not a directory" ; \
		exit 1 ; \
	fi
	rm -rf ./orig
	mkdir ./orig
	tar cbf 20 - `cat mstfiles` | (cd ./orig; tar xbBf 20 -)

