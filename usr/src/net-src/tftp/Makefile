CC = gcc
PROG = tftp
OBJS = tftp.o main.o tftpsubs.o
CFLAGS = -O2 -include ../bsd/bsd.h -I../bsd -Dsin=x_sin
LDFLAGS = -N
LDLIBS = ../libbsd/libbsd.a

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
