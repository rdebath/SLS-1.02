CC = gcc
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd -DSHADOW_PWD
LDFLAGS =
LDLIBS = ../libbsd/libbsd.a -lshadow
PROG = inetd

all: $(PROG)

inetd: inetd.o

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)

