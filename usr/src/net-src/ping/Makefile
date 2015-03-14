CC = gcc
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd
LDFLAGS = -N
PROG = ping

all: $(PROG)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)

