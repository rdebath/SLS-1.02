PROG = nstest
CC = gcc
CFLAGS = -O2 -include ../../../bsd/bsd.h -I../../../bsd
LDFLAGS = -N
LDLIBS = ../../../libbsd/libbsd.a

all: $(PROG)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)

