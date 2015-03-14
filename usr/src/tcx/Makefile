#
#		Makefile for tcx
#
#	Author : Stewart Forster, University Of Melbourne, 25/31993
#

# C compiler of your choice.  Should be ansi'ish
CC = cc			# IRIX, ULTRIX, AIX
#CC = gcc		# SUNOS


# Cflags of choice.
CFLAGS = -s -O		# Most OS's
#CFLAGS = -s -O6 -fomit-frame-pointer -Wall	# Linux


# Any libraries
LIBS = -lmalloc -lc_s	# IRIX
#LIBS =			# SUNOS, ULTRIX, AIX


all:	tcx untcx

tcx:	tcx.c config.h
	$(CC) $(CFLAGS) -o tcx tcx.c $(LIBS)

untcx:	untcx.c config.h
	$(CC) $(CFLAGS) -o untcx untcx.c $(LIBS)

shar:
	shar -c -l 45 -o tcx README VERSION COPYING Makefile tcx.1 untcx.1 tcx.c config.h untcx.c

clean:
	/bin/rm -f *.o tcx untcx
