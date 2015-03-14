CC = gcc
CFLAGS = -O2 -Wall -D__USE_BSD_SIGNAL
LDFLAGS = -N
LDLIBS = # -lrpclib

PROG = portmap

all: $(PROG)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)

