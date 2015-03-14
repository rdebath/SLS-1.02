CC = gcc
PROG = fingerd
OBJS = fingerd.o
CFLAGS = -O2 # -include ../bsd/bsd.h -I../bsd -DSHADOW_PWD
LDFLAGS = -N
LDLIBS = # ../libbsd/libbsd.a -lshadow

all: $(PROG)

$(PROG): $(OBJS)

clean:
	rm -f *.o *~ core

clobber: clean
	rm -f $(PROG)
