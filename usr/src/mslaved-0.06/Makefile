#
# Makefile for mslaved, the MandelSpawn computation server
#
# This does not use Imake for two reasons:
# - the server is completely independent of X and may be compiled and 
#   used on a machine that does not have X installed
# - I don't know how to install man pages in section 8 using Imake
#

# Edit these as necessary
BINDIR=/usr/local/bin
DAEMONDIR=/usr/local/etc

# Make sure you use gcc if you have a Vax, 680x0 or i386 and
# you don't have a high-performance floating point unit.  
# If compiled without gcc on those machines, mslaved will use floating 
# point instead of fixed point, and any old PC Mandelbrot program
# is likely to run circles around your fancy workstation network.
# For machines with fast floating point, such as many Sparc- or
# MIPS-based workstations, gcc won't make much of a difference and
# may even be slower.
CC=gcc
LD=$(CC)

CFLAGS= -O
LDFLAGS=

BINARIES= mslaved mslavedc

# Non-BSD machines usually need something like -linet or -lsocket here
LIBS=

all: $(BINARIES)

mslaved: mslaved.c ms_ipc.h ms_real.c ms_real.h ms_job.h
	$(CC) $(CFLAGS) mslaved.c $(LIBS) -o mslaved

mslavedc: mslavedc.c ms_ipc.h
	$(CC) $(CFLAGS) mslavedc.c $(LIBS) -o mslavedc

# Also remember to edit /etc/inetd.conf and /etc/services as described in
# the INSTALL file.
install:
	-mkdir $(DAEMONDIR)
	cp mslaved $(DAEMONDIR)/mslaved; strip $(DAEMONDIR)/mslaved
	cp mslavedc $(DAEMONDIR)/mslavedc; strip $(DAEMONDIR)/mslavedc

clean:
	rm -f core mslaved mslavedc *.o *~
