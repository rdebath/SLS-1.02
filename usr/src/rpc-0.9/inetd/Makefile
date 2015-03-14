CC = gcc -I.
CFLAGS = -O2
LDFLAGS =
LDLIBS = 
OBJS = inetd.o daemon.o
PROG = inetd

all: $(PROG)

inetd: $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)

